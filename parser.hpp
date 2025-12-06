#pragma once

#include <cstdlib>
#include <memory>
#include <print>
#include <string>
#include <utility>
#include <vector>

#include <llvm/IR/Value.h>

#include "lexer.hpp"

class ExprVisitor {
public:
  virtual ~ExprVisitor() = default;
  virtual llvm::Value *visit(class NumberExprAST &node) = 0;
  virtual llvm::Value *visit(class VariableExprAST &node) = 0;
  virtual llvm::Value *visit(class BinaryExprAST &node) = 0;
  virtual llvm::Value *visit(class CallExprAST &node) = 0;
  virtual llvm::Value *visit(class IfExprAST &node) = 0;
  virtual llvm::Value *visit(class ForExprAST &node) = 0;
};

class FunctionVisitor {
public:
  virtual ~FunctionVisitor() = default;
  virtual llvm::Function *visit(class PrototypeAST &node) = 0;
  virtual llvm::Function *visit(class FunctionAST &node) = 0;
};

class ExprAST {
public:
  virtual ~ExprAST() = default;
  virtual llvm::Value *accept(ExprVisitor &v) = 0;
};

class NumberExprAST : public ExprAST {
  double Val;

public:
  NumberExprAST(double Val) : Val(Val) {}
  double getValue() const { return Val; }
  llvm::Value *accept(ExprVisitor &v) override { return v.visit(*this); }
};

class VariableExprAST : public ExprAST {
  std::string Name;

public:
  const std::string &getName() const { return Name; }
  VariableExprAST(const std::string &Name) : Name(Name) {}
  llvm::Value *accept(ExprVisitor &v) override { return v.visit(*this); }
};

class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  char getOp() const { return Op; }
  ExprAST *getLHS() const { return LHS.get(); }
  ExprAST *getRHS() const { return RHS.get(); }
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  llvm::Value *accept(ExprVisitor &v) override { return v.visit(*this); }
};

class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  const std::string &getCallee() const { return Callee; }
  const std::vector<std::unique_ptr<ExprAST>> &getArgs() const { return Args; }
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> &Args)
      : Callee(Callee), Args(std::move(Args)) {}
  llvm::Value *accept(ExprVisitor &v) override { return v.visit(*this); }
};

class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;

public:
  const std::string &getName() const { return Name; }
  const std::vector<std::string> &getArgs() const { return Args; }
  PrototypeAST(const std::string &Name, const std::vector<std::string> &Args)
      : Name(Name), Args(Args) {}
  llvm::Function *accept(FunctionVisitor &v) { return v.visit(*this); };
};

class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

public:
  // const PrototypeAST* getProto() const { return Proto.get(); }
  // const ExprAST* getBody() const { return Body.get(); }
  const std::unique_ptr<PrototypeAST> &getProto() const { return Proto; }
  const std::unique_ptr<ExprAST> &getBody() const { return Body; }
  std::unique_ptr<PrototypeAST> releaseProto() { return std::move(Proto); }
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}
  llvm::Function *accept(FunctionVisitor &v) { return v.visit(*this); };
};

static int currentToken;
static int GetNextToken() {
  currentToken = GetToken();
  // std::println("Got token: {}", tokenNames.contains(currentToken)
  //                                   ? tokenNames.at(currentToken)
  //                                   : std::to_string(currentToken));
  return currentToken;
}

class IfExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Cond, Then, Else;

public:
  ExprAST *getCond() const { return Cond.get(); }
  ExprAST *getThen() const { return Then.get(); }
  ExprAST *getElse() const { return Else.get(); }
  IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
            std::unique_ptr<ExprAST> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
  llvm::Value *accept(ExprVisitor &v) override { return v.visit(*this); }
};

class ForExprAST : public ExprAST {
  std::string VarName;
  std::unique_ptr<ExprAST> Start, End, Step, Body;

public:
  const std::string &getVarName() const { return VarName; }
  ExprAST *getStart() const { return Start.get(); }
  ExprAST *getEnd() const { return End.get(); }
  ExprAST *getStep() const { return Step.get(); }
  ExprAST *getBody() const { return Body.get(); }
  ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
             std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
             std::unique_ptr<ExprAST> Body)
      : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
        Step(std::move(Step)), Body(std::move(Body)) {}
  llvm::Value *accept(ExprVisitor &v) override { return v.visit(*this); }
};

llvm::Value *LogErrorV(const char *Str) {
  std::println("Error: {}", Str);
  return nullptr;
}

std::unique_ptr<ExprAST> LogError(const char *Str) {
  std::println("Error: {}", Str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(numVal);
  GetNextToken();
  return std::move(Result);
}

static std::unique_ptr<ExprAST> ParseExpression();

static std::unique_ptr<ExprAST> ParseIfExpr() {
  GetNextToken(); // eat the 'if'.

  auto Cond = ParseExpression();
  if (!Cond) {
    return nullptr;
  }

  if (currentToken != std::to_underlying(Token::THEN)) {
    return LogError("expected 'then'");
  }
  GetNextToken(); // eat the 'then'

  auto Then = ParseExpression();
  if (!Then) {
    return nullptr;
  }

  if (currentToken != std::to_underlying(Token::ELSE)) {
    return LogError("expected 'else'");
  }
  GetNextToken(); // eat the 'else'

  auto Else = ParseExpression();
  if (!Else) {
    return nullptr;
  }

  return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                     std::move(Else));
}

static std::unique_ptr<ExprAST> ParseForExpr() {
  GetNextToken(); // eat the 'for'.

  if (currentToken != static_cast<int>(Token::IDENTIFIER)) {
    return LogError("expected identifier after for");
  }

  std::string IdName = identifier;
  GetNextToken(); // eat identifier.

  if (currentToken != '=') {
    return LogError("expected '=' after for");
  }
  GetNextToken(); // eat '='.

  auto Start = ParseExpression();
  if (!Start) {
    return nullptr;
  }

  if (currentToken != ',') {
    return LogError("expected ',' after for start value");
  }
  GetNextToken(); // eat ','.

  auto End = ParseExpression();
  if (!End) {
    return nullptr;
  }

  if (currentToken != ',') {
    return LogError("expected ',' after for end value");
  }
  GetNextToken(); // eat ','.

  if (currentToken != static_cast<int>(Token::IDENTIFIER)) {
    return LogError("expected identifier after for end");
  }
  GetNextToken(); // eat identifier.

  if (currentToken != '=') {
    return LogError("expected '=' after identifier increment expression");
  }
  GetNextToken(); // eat '='.

  if (currentToken != static_cast<int>(Token::IDENTIFIER)) {
    return LogError("expected identifier after for end");
  }
  GetNextToken(); // eat identifier.

  if (currentToken != '+') {
    return LogError("expected '+' for increment expression");
  }
  GetNextToken(); // eat '+'.

  std::unique_ptr<ExprAST> Step;
  Step = ParseExpression();
  if (!Step) {
    return nullptr;
  }

  if (currentToken != std::to_underlying(Token::DO)) {
    return LogError("expected 'do' after for");
  }
  GetNextToken(); // eat 'do'.

  auto Body = ParseExpression();
  if (!Body) {
    return nullptr;
  }

  return std::make_unique<ForExprAST>(IdName, std::move(Start), std::move(End),
                                      std::move(Step), std::move(Body));
}

// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
  GetNextToken(); // eat (.
  auto V = ParseExpression();
  if (!V) {
    return nullptr;
  }
  if (currentToken != ')') {
    return LogError("expected ')'");
  }
  GetNextToken(); // eat ).
  return V;
}

// identifierexpr
//   ::= identifier
//   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = identifier;
  GetNextToken();            // eat identifier.
  if (currentToken != '(') { // Simple variable ref.
    return std::make_unique<VariableExprAST>(IdName);
  }

  // Call.
  GetNextToken(); // eat (.
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (currentToken != ')') {
    while (true) {
      if (auto Arg = ParseExpression()) {
        Args.push_back(std::move(Arg));
      } else {
        return nullptr;
      }

      if (currentToken == ')') {
        break;
      }
      if (currentToken != ',') {
        return LogError("Expected ')' or ',' in argument list");
      }
      GetNextToken();
    }
  }

  GetNextToken(); // eat ).
  return std::make_unique<CallExprAST>(IdName, Args);
}

// primary
//   ::= identifierexpr
//   ::= numberexpr
//   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (currentToken) {
  case std::to_underlying(Token::IDENTIFIER):
    return ParseIdentifierExpr();
  case std::to_underlying(Token::NUMBER):
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  case std::to_underlying(Token::IF):
    return ParseIfExpr();
  case std::to_underlying(Token::FOR):
    return ParseForExpr();
  default:
    return LogError("unknown token when expecting an expression");
  }
}

static std::unordered_map<char, int> BinopPrecedence;

static int GetTokenPrecedence() {
  if (!isascii(currentToken)) {
    return -1;
  }
  int TokPrec = BinopPrecedence[currentToken];
  if (TokPrec <= 0) {
    return -1;
  }
  return TokPrec;
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
  // std::println("Parsing binary operation RHS with precedence {}", ExprPrec);
  while (true) {
    int tokPrec = GetTokenPrecedence();
    if (tokPrec < ExprPrec) {
      return LHS;
    }

    int BinOp = currentToken;
    GetNextToken(); // eat binop

    auto RHS = ParsePrimary();
    if (!RHS) {
      return nullptr;
    }

    int nextPrec = GetTokenPrecedence();
    if (tokPrec < nextPrec) {
      RHS = ParseBinOpRHS(tokPrec + 1, std::move(RHS));
      if (!RHS) {
        return nullptr;
      }
    }

    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS) {
    return nullptr;
  }
  return ParseBinOpRHS(0, std::move(LHS));
}

// prototype
// ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  if (currentToken != static_cast<int>(Token::IDENTIFIER)) {
    return LogErrorP("Expected function name in prototype");
  }

  std::string fnName = identifier;
  GetNextToken();

  if (currentToken != '(') {
    return LogErrorP("Expected '(' in prototype");
  }

  std::vector<std::string> argNames;
  while (GetNextToken() == static_cast<int>(Token::IDENTIFIER)) {
    argNames.push_back(identifier);
  }
  if (currentToken != ')') {
    return LogErrorP("Expected ')' in prototype");
  }

  GetNextToken(); // eat ')'

  return std::make_unique<PrototypeAST>(fnName, std::move(argNames));
}

// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
  GetNextToken(); // eat def.
  auto Proto = ParsePrototype();
  if (!Proto) {
    return nullptr;
  }

  if (auto expr = ParseExpression()) {
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(expr));
  }
  return nullptr;
}

// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() {
  GetNextToken(); // eat extern.
  return ParsePrototype();
}

static unsigned AnonymousExprCount = 0;

// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto expr = ParseExpression()) {
    auto Proto = std::make_unique<PrototypeAST>(
        "__anon_expr" + std::to_string(AnonymousExprCount++),
        std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(expr));
  }
  return nullptr;
}

// top level parsing

// static void HandleDefinition() {
//   if (ParseDefinition()) {
//     std::println("Parsed a function definition.");
//   } else {
//     GetNextToken();
//   }
// }

// static void HandleExtern() {
//   if (ParseExtern()) {
//     std::println("Parsed an extern");
//   } else {
//     GetNextToken();
//   }
// }

// static void HandleTopLevelExpression() {
//   if (ParseTopLevelExpr()) {
//     std::println("Parsed a top-level expr");
//   } else {
//     GetNextToken();
//   }
// }

// static void MainLoop() {
//   while (true) {
//     std::print(">>> ");
//     switch (currentToken) {
//     case static_cast<int>(Token::ENOF):
//       return;
//     case ';': // ignore top-level semicolons.
//       GetNextToken();
//       break;
//     case static_cast<int>(Token::DEF):
//       HandleDefinition();
//       break;
//     case static_cast<int>(Token::EXTERN):
//       HandleExtern();
//       break;
//     default:
//       HandleTopLevelExpression();
//       break;
//     }
//   }
// }

// int main() {

//   BinopPrecedence['<'] = 10;
//   BinopPrecedence['+'] = 20;
//   BinopPrecedence['-'] = 20;
//   BinopPrecedence['*'] = 40; // highest.

//   std::print(">>> ");
//   GetNextToken();
//   MainLoop();
// }