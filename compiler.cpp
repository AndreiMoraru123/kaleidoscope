#include "parser.hpp"

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>

using namespace llvm;

static std::unique_ptr<LLVMContext> theContext;
static std::unique_ptr<IRBuilder<>> builder;
static std::unique_ptr<Module> theModule;
static std::map<std::string, Value *> namedValues;

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
    Function *callee = theModule->getFunction(node.getCallee());
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
    Function *function = theModule->getFunction(node.getProto()->getName());
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

    CodegenExprVisitor exprVisitor;
    if (Value *retVal = node.getBody()->accept(exprVisitor)) {
      builder->CreateRet(retVal);
      verifyFunction(*function);
      return function;
    }

    function->eraseFromParent();
    return nullptr;
  }
};

static void InitializeModule() {
  theContext = std::make_unique<LLVMContext>();
  theModule = std::make_unique<Module>("my cool jit", *theContext);
  builder = std::make_unique<IRBuilder<>>(*theContext);
}

CodegenFunctionVisitor funcVisitor;

static void HandleDefinition() {
  if (auto funcAST = ParseDefinition()) {
    if (Function *funcIR = funcAST->accept(funcVisitor)) {
      std::println("Parsed a function definition:");
      funcIR->print(llvm::outs());
      std::println();
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
    }
  } else {
    GetNextToken();
  }
}

static void HandleTopLevelExpression() {
  if (auto funcAST = ParseTopLevelExpr()) {
    if (Function *funcIR = funcAST->accept(funcVisitor)) {
      std::println("Parsed a top-level expr:");
      funcIR->print(llvm::outs());
      std::println();
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
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest.

  // Prime the first token.
  std::print(">>> ");
  GetNextToken();

  // Make the module, which holds all the code.
  InitializeModule();

  // Run the main "interpreter loop" now.
  MainLoop();

  // Print out all of the generated code.
  theModule->print(errs(), nullptr);

  return 0;
}