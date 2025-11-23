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
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>

using namespace llvm;

static std::unique_ptr<LLVMContext> theContext;
static std::unique_ptr<Module> theModule;
static std::unique_ptr<IRBuilder<>> builder;
static std::map<std::string, Value *> namedValues;
static std::unique_ptr<KaleidoscopeJIT> theJIT;
static std::unique_ptr<FunctionPassManager> theFPM;
static std::unique_ptr<LoopAnalysisManager> theLAM;
static std::unique_ptr<FunctionAnalysisManager> theFAM;
static std::unique_ptr<CGSCCAnalysisManager> theCGAM;
static std::unique_ptr<ModuleAnalysisManager> theMAM;
static std::unique_ptr<PassInstrumentationCallbacks> thePIC;
static std::unique_ptr<StandardInstrumentations> theSI;
static std::map<std::string, std::unique_ptr<PrototypeAST>> functionProtos;

Function *getFunction(std::string Name);

class CodegenExprVisitor : public ExprVisitor {
public:
  Value *visit(NumberExprAST &node) {
    return ConstantFP::get(*theContext, APFloat(node.getValue()));
  }

  Value *visit(VariableExprAST &node) {
    Value *V = namedValues[node.getName()];
    if (!V) {
      LogErrorV("Unknown variable name");
      return nullptr;
    }
    return V;
  }

  Value *visit(BinaryExprAST &node) {
    Value *L = node.getLHS()->accept(*this);
    Value *R = node.getRHS()->accept(*this);
    if (!L || !R) {
      return nullptr;
    }

    switch (node.getOp()) {
    case '+':
      return builder->CreateFAdd(L, R, "addtmp");
    case '-':
      return builder->CreateFSub(L, R, "subtmp");
    case '*':
      return builder->CreateFMul(L, R, "multmp");
    case '/':
      return builder->CreateFDiv(L, R, "divtmp");
    default:
      LogErrorV("invalid binary operator");
      return nullptr;
    }
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
    // Function *function = theModule->getFunction(node.getProto()->getName());
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
      namedValues[std::string(arg.getName())] = &arg;
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

  return 0;
}