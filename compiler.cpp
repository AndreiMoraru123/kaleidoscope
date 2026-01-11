#include "jit.hpp"
#include "parser.hpp"

#include <cassert>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>
#include <llvm/MC/TargetRegistry.h>


using namespace llvm;

static std::unique_ptr<LLVMContext> theContext;
static std::unique_ptr<Module> theModule;
static std::unique_ptr<IRBuilder<>> builder;
static std::map<std::string, AllocaInst *> namedValues;
static std::unique_ptr<KaleidoscopeJIT> theJIT;
static std::unique_ptr<FunctionPassManager> theFPM;
static std::unique_ptr<LoopAnalysisManager> theLAM;
static std::unique_ptr<FunctionAnalysisManager> theFAM;
static std::unique_ptr<CGSCCAnalysisManager> theCGAM;
static std::unique_ptr<ModuleAnalysisManager> theMAM;
static std::unique_ptr<PassInstrumentationCallbacks> thePIC;
static std::unique_ptr<StandardInstrumentations> theSI;
static std::map<std::string, std::unique_ptr<PrototypeAST>> functionProtos;


static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                         const std::string &VarName) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(Type::getDoubleTy(*theContext), nullptr, VarName.c_str());
}

Function *getFunction(std::string Name);

class CodegenExprVisitor : public ExprVisitor {
public:
  Value *visit(NumberExprAST &node) {
    return ConstantFP::get(*theContext, APFloat(node.getValue()));
  }

  Value *visit(VariableExprAST &node) {
    AllocaInst *A = namedValues[node.getName()];
    if (!A) {
      LogErrorV("Unknown variable name");
      return nullptr;
    }
    return builder->CreateLoad(A->getAllocatedType(), A, node.getName().c_str());
  }
  
  Value *visit(UnaryExprAST &node) {
    Value *operandV = node.getOperand()->accept(*this);
    if (!operandV) {
      return nullptr;
    }

    Function *F = getFunction(std::string("unary") + node.getOp());
    if (!F) {
      LogErrorV("Unknown unary operator");
      return nullptr;
    }

    return builder->CreateCall(F, operandV, "unop");
  }

  Value *visit(BinaryExprAST &node) {
    Value *L = node.getLHS()->accept(*this);
    Value *R = node.getRHS()->accept(*this);
    if (!L || !R) {
      return nullptr;
    }

    switch (node.getOp()) {
      case '=': {
        // Assignment requires the LHS to be an identifier.
        VariableExprAST *LHSE = static_cast<VariableExprAST *>(node.getLHS());
        if (!LHSE) {
          LogErrorV("destination of '=' must be a variable");
          return nullptr;
        }

        // Codegen the RHS.
        Value *Val = R;
        if (!Val) {
          return nullptr;
        }

        // Look up the name.
        Value *Variable = namedValues[LHSE->getName()];
        if (!Variable) {
          LogErrorV("Unknown variable name");
          return nullptr;
        }

        builder->CreateStore(Val, Variable);
        return Val;
      }
    case '+':
      return builder->CreateFAdd(L, R, "addtmp");
    case '-':
      return builder->CreateFSub(L, R, "subtmp");
    case '*':
      return builder->CreateFMul(L, R, "multmp");
    case '/':
      return builder->CreateFDiv(L, R, "divtmp");
    case '<': {
      L = builder->CreateFCmpULT(L, R, "cmptmp");
      // Convert bool 0/1 to double 0.0 or 1.0
      return builder->CreateUIToFP(L, Type::getDoubleTy(*theContext),
                                   "booltmp");
    }
    default:
      break;
    }
    
    Function *F = getFunction(std::string("binary") + node.getOp()); 
    assert(F && "Binary operator function not found");
    
    Value *ops[] = {L, R};
    return builder->CreateCall(F, ops, "binop");
  }

  Value *visit(CallExprAST &node) {
    // Look up the name in the global module table.
    // Function *callee = theModule->getFunction(node.getCallee());
    Function *callee = getFunction(node.getCallee());
    if (!callee) {
      LogErrorV("Unknown function referenced");
      return nullptr;
    }

    // If argument mismatch error.
    if (callee->arg_size() != node.getArgs().size()) {
      LogErrorV("Incorrect # arguments passed");
      return nullptr;
    }

    std::vector<Value *> ArgsV;
    for (unsigned i = 0, e = node.getArgs().size(); i != e; ++i) {
      ArgsV.push_back(node.getArgs()[i]->accept(*this));
      if (!ArgsV.back()) {
        return nullptr;
      }
    }

    return builder->CreateCall(callee, ArgsV, "calltmp");
  }

  Value *visit(IfExprAST &node) {
    Value *condV = node.getCond()->accept(*this);
    if (!condV) {
      return nullptr;
    }

    // Convert condition to a bool by comparing non-equal to 0.0.
    condV = builder->CreateFCmpONE(
        condV, ConstantFP::get(*theContext, APFloat(0.0)), "ifcond");

    Function *theFunction = builder->GetInsertBlock()->getParent();

    // Create blocks for the then and else cases. Insert the 'then' block at the
    // end of the function.
    BasicBlock *thenBB = BasicBlock::Create(*theContext, "then", theFunction);
    BasicBlock *elseBB = BasicBlock::Create(*theContext, "else");
    BasicBlock *mergeBB = BasicBlock::Create(*theContext, "ifcont");

    builder->CreateCondBr(condV, thenBB, elseBB);

    // Emit then value.
    builder->SetInsertPoint(thenBB);

    Value *thenV = node.getThen()->accept(*this);
    if (!thenV) {
      return nullptr;
    }

    builder->CreateBr(mergeBB);
    // Codegen of 'Then' can change the current block, update thenBB for the
    // PHI.
    thenBB = builder->GetInsertBlock();

    // Emit else block.
    theFunction->insert(theFunction->end(), elseBB);
    builder->SetInsertPoint(elseBB);

    Value *elseV = node.getElse()->accept(*this);
    if (!elseV) {
      return nullptr;
    }
    builder->CreateBr(mergeBB);
    // Codegen of 'Then' can change the current block, update thenBB for the
    // PHI.
    elseBB = builder->GetInsertBlock();

    // Emit merge block.
    theFunction->insert(theFunction->end(), mergeBB);
    builder->SetInsertPoint(mergeBB);
    PHINode *pn =
        builder->CreatePHI(Type::getDoubleTy(*theContext), 2, "iftmp");

    pn->addIncoming(thenV, thenBB);
    pn->addIncoming(elseV, elseBB);
    return pn;
  };

  Value *visit(ForExprAST &node) {
    // Make the new basic block for the loop header, inserting after current block.
    Function *theFunction = builder->GetInsertBlock()->getParent();
    
    // Create an alloca for the variable in the entry block.
    AllocaInst *alloca = CreateEntryBlockAlloca(theFunction, node.getVarName());

    // Emit the start code first, without 'variable' in scope.
    Value *startV = node.getStart()->accept(*this);
    if (!startV) {
      return nullptr;
    }

    // Store the start value into the alloca.
    builder->CreateStore(startV, alloca);
    
    BasicBlock *preheaderBB = builder->GetInsertBlock();
    BasicBlock *loopBB = BasicBlock::Create(*theContext, "loop", theFunction);

    // Insert an explicit fall through from the current block to the loopBB.
    builder->CreateBr(loopBB);

    // Start insertion in loopBB.
    builder->SetInsertPoint(loopBB);

    // Within the loop, the variable is defined equal to the PHI node. If it
    // shadows an existing variable, we have to restore it, so save it now.
    AllocaInst *oldVal = namedValues[node.getVarName()];
    namedValues[node.getVarName()] = alloca;

    // Emit the body of the loop. This, like any other expr, can change the
    // current BB. Note that we ignore the value computed by the body, but
    // don't allow an error.
    if (!node.getBody()->accept(*this)) {
      return nullptr;
    }

    // Emit the step value.
    Value *stepV = nullptr;
    if (node.getStep()) {
      stepV = node.getStep()->accept(*this);
      if (!stepV) {
        return nullptr;
      }
    } else {
      // If not specified, it's an error.
      LogErrorV("for loop step value required");
    }

    // Compute the end condition.
    Value *endCond = node.getEnd()->accept(*this);
    if (!endCond) {
      return nullptr;
    }

    // Reload, increment, and restore the alloca. This handles the case where
    // the body of the loop mutates the variable.
    Value* currVar = builder->CreateLoad(alloca->getAllocatedType(), alloca, node.getVarName().c_str());
    Value *nextVar = builder->CreateFAdd(currVar, stepV, "nextvar");
    builder->CreateStore(nextVar, alloca);

    // Convert condition to a bool by comparing non-equal to 0.0.
    endCond = builder->CreateFCmpONE(
        endCond, ConstantFP::get(*theContext, APFloat(0.0)), "loopcond");

    // Create the "after loop" block and insert it.
    BasicBlock *loopEndBB = builder->GetInsertBlock();
    BasicBlock *afterBB =
        BasicBlock::Create(*theContext, "afterloop", theFunction);

    // Insert the conditional branch into the end of loopEndBB.
    builder->CreateCondBr(endCond, loopBB, afterBB);

    // Any new code will be inserted in afterBB.
    builder->SetInsertPoint(afterBB);

    // Restore the unshadowed variable.
    if (oldVal) {
      namedValues[node.getVarName()] = oldVal;
    } else {
      namedValues.erase(node.getVarName());
    }

    // for expr always returns 0.0.
    return Constant::getNullValue(Type::getDoubleTy(*theContext));
  };
  
  Value *visit(VarExprAST &node) {
    std::vector<AllocaInst *> oldBindings;

    Function *theFunction = builder->GetInsertBlock()->getParent();

    // Register all variables and emit their initializer.
    for (unsigned i = 0, e = node.getVarNames().size(); i != e; ++i) {
      const auto &varPair = node.getVarNames()[i];
      const std::string &varName = varPair.first;
      ExprAST *init = varPair.second.get();

      // Emit the initializer before creating the alloca. This prevents the
      // initializer from referencing the variable itself, and permits stuff
      // like this:
      //  var a = 1 in
      //    var a = a in ...   # refers to outer 'a'.
      Value *initVal;
      if (init) {
        initVal = init->accept(*this);
        if (!initVal) {
          return nullptr;
        }
      } else {
        // If not specified, use 0.0.
        initVal = ConstantFP::get(*theContext, APFloat(0.0));
      }

      AllocaInst *alloca = CreateEntryBlockAlloca(theFunction, varName);
      builder->CreateStore(initVal, alloca);

      // Remember the old variable binding so that we can restore it later.
      oldBindings.push_back(namedValues[varName]);

      // Remember this binding.
      namedValues[varName] = alloca;
    }

    // Codegen the body of the let expression.
    Value *bodyV = node.getBody()->accept(*this);
    if (!bodyV) {
      return nullptr;
    }

    // Restore all old bindings.
    for (unsigned i = 0, e = node.getVarNames().size(); i != e; ++i) {
      const std::string &varName = node.getVarNames()[i].first;
      namedValues[varName] = oldBindings[i];
    }

    // Return the body computation.
    return bodyV;
  };
};

class CodegenFunctionVisitor : public FunctionVisitor {
  CodegenExprVisitor exprVisitor;

public:
  Function *visit(PrototypeAST &node) {
    std::vector<Type *> Doubles(node.getArgs().size(),
                                Type::getDoubleTy(*theContext));
    FunctionType *FT =
        FunctionType::get(Type::getDoubleTy(*theContext), Doubles, false);
    Function *F = Function::Create(FT, Function::ExternalLinkage,
                                   node.getName(), theModule.get());

    unsigned idx = 0;
    for (auto &arg : F->args()) {
      arg.setName(node.getArgs()[idx++]);
    }
    return F;
  }

  Function *visit(FunctionAST &node) {
    Function *function = getFunction(node.getProto()->getName());
    if (!function) {
      function = node.getProto()->accept(*this);
    } else {
      // validate signature matches
      if (function->arg_size() != node.getProto()->getArgs().size()) {
        LogErrorV("Function redefinition with different # args");
        return nullptr;
      }

      // update argument names to match new definition
      unsigned idx = 0;
      for (auto &arg : function->args()) {
        arg.setName(node.getProto()->getArgs()[idx++]);
      }
    }

    const std::string protoName = node.getProto()->getName();
    
    if (node.getProto()->isBinaryOp()) {
      const auto precedence = node.getProto()->getPrecedence();
      BinopPrecedence[node.getProto()->getOperatorName()] = precedence;
    }

    functionProtos[protoName] = node.releaseProto();

    if (!function) {
      return nullptr;
    }

    if (!function->empty()) {
      LogErrorV("Function cannot be redefined.");
      return nullptr;
    }

    BasicBlock *BB = BasicBlock::Create(*theContext, "entry", function);
    builder->SetInsertPoint(BB);

    namedValues.clear();
    for (auto &arg : function->args()) {
      AllocaInst *alloca = CreateEntryBlockAlloca(function, std::string(arg.getName()));
      builder->CreateStore(&arg, alloca);
      namedValues[std::string(arg.getName())] = alloca;
    }

    if (Value *retVal = node.getBody()->accept(exprVisitor)) {
      builder->CreateRet(retVal);
      verifyFunction(*function);
      theFPM->run(*function, *theFAM);
      return function;
    }

    function->eraseFromParent();
    return nullptr;
  }
};

static void InitializeModuleAndManagers() {
  theContext = std::make_unique<LLVMContext>();
  theModule = std::make_unique<Module>("my cool jit", *theContext);
  theModule->setDataLayout(theJIT->getDataLayout());

  builder = std::make_unique<IRBuilder<>>(*theContext);

  theFPM = std::make_unique<FunctionPassManager>();
  theLAM = std::make_unique<LoopAnalysisManager>();
  theFAM = std::make_unique<FunctionAnalysisManager>();
  theCGAM = std::make_unique<CGSCCAnalysisManager>();
  theMAM = std::make_unique<ModuleAnalysisManager>();
  thePIC = std::make_unique<PassInstrumentationCallbacks>();
  theSI = std::make_unique<StandardInstrumentations>(*theContext,
                                                     /*DebugLogging*/ true);
  theSI->registerCallbacks(*thePIC, theMAM.get());

  // Add transform passes.
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  theFPM->addPass(InstCombinePass());
  // Reassociate expressions.
  theFPM->addPass(ReassociatePass());
  // Eliminate Common SubExpressions.
  theFPM->addPass(GVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  theFPM->addPass(SimplifyCFGPass());
  // Promote allocas to registers.
  theFPM->addPass(PromotePass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  theFPM->addPass(InstCombinePass());
  // Reassociate expressions.
  theFPM->addPass(ReassociatePass());

  PassBuilder PB;
  PB.registerModuleAnalyses(*theMAM);
  PB.registerFunctionAnalyses(*theFAM);
  PB.crossRegisterProxies(*theLAM, *theFAM, *theCGAM, *theMAM);
}

CodegenFunctionVisitor funcVisitor;

Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = theModule->getFunction(Name))
    return F;

  // If not, check whether we can codegen the declaration from some existing
  // prototype.
  auto FI = functionProtos.find(Name);
  if (FI != functionProtos.end())
    return FI->second->accept(funcVisitor);

  // If no existing prototype exists, return null.
  return nullptr;
}

static ExitOnError exitOnError;

static void HandleDefinition() {
  if (auto funcAST = ParseDefinition()) {
    if (Function *funcIR = funcAST->accept(funcVisitor)) {
      std::println("Parsed a function definition:");
      funcIR->print(llvm::outs());
      std::println();
      exitOnError(theJIT->addModule(
          ThreadSafeModule(std::move(theModule), std::move(theContext))));
      InitializeModuleAndManagers();
    }
  } else {
    GetNextToken();
  }
}

static void HandleExtern() {
  if (auto protoAST = ParseExtern()) {
    if (Function *funcIR = protoAST->accept(funcVisitor)) {
      std::println("Parsed an extern:");
      funcIR->print(llvm::outs());
      std::println();
      functionProtos[protoAST->getName()] = std::move(protoAST);
    }
  } else {
    GetNextToken();
  }
}

static void HandleTopLevelExpression() {
  if (auto funcAST = ParseTopLevelExpr()) {
    if (Function *funcIR = funcAST->accept(funcVisitor)) {

      // Create a ResourceTracker to track JIT'd memory allocated to our
      // anonymous expression -- that way we can free it after executing.
      auto rt = theJIT->getMainJITDylib().createResourceTracker();

      std::println("Parsed a top-level expr:");
      funcIR->print(llvm::outs());
      std::println();

      auto tsm = ThreadSafeModule(std::move(theModule), std::move(theContext));
      exitOnError(theJIT->addModule(std::move(tsm), rt));
      InitializeModuleAndManagers();

      // Search the JIT for the __anon_expr symbol.
      auto exprSymbol = exitOnError(theJIT->lookup(
          "__anon_expr" + std::to_string(AnonymousExprCount - 1)));

      double (*FP)() = exprSymbol.getAddress().toPtr<double (*)()>();
      std::println("Evaluated to {}", FP());

      exitOnError(rt->remove());
    }
  } else {
    GetNextToken();
  }
}

static void MainLoop() {
  while (true) {
    std::print(">>> ");
    switch (currentToken) {
    case static_cast<int>(Token::ENOF):
      return;
    case ';': // ignore top-level semicolons.
      GetNextToken();
      break;
    case static_cast<int>(Token::DEF):
      HandleDefinition();
      break;
    case static_cast<int>(Token::EXTERN):
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

int main() {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  BinopPrecedence['='] = 2;
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest.

  // Prime the first token.
  std::print(">>> ");
  GetNextToken();

  theJIT = exitOnError(KaleidoscopeJIT::Create());

  // Make the module, which holds all the code.
  InitializeModuleAndManagers();

  // Run the main "interpreter loop" now.
  MainLoop();
  
  // Initialize the target registry etc.
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmParsers();
  InitializeAllAsmPrinters();

  auto TargetTriple = LLVMGetDefaultTargetTriple();
  theModule->setTargetTriple(Triple(TargetTriple));
  
  std::string Error;
  auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the requested target.
  if (!Target) {
    errs() << Error;
    return 1;
  }
  
  auto CPU = "generic";
  auto Features = "";
  
  TargetOptions opt;
  auto TheTargetMachine =
      Target->createTargetMachine(TargetTriple, CPU, Features, opt, Reloc::PIC_);
  
  theModule->setDataLayout(TheTargetMachine->createDataLayout());
  auto Filename = "output.o";;
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::OF_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }

  legacy::PassManager pass;
  auto FileType = CodeGenFileType::ObjectFile;
  
  if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    errs() << "TheTargetMachine can't emit a file of this type";
    return 1;
  }

  pass.run(*theModule);
  dest.flush();

  outs() << "Wrote " << Filename << "\n";

  return 0;
}