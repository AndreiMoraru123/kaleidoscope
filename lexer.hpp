#pragma once

#include <cctype>
#include <cstdio>
#include <map>
#include <string>

enum class Token : int {
  ENOF = -1,
  DEF = -2,
  EXTERN = -3,
  IDENTIFIER = -4,
  NUMBER = -5,
};

const inline std::map<int, std::string> tokenNames = {
    {static_cast<int>(Token::ENOF), "EOF"},
    {static_cast<int>(Token::DEF), "DEF"},
    {static_cast<int>(Token::EXTERN), "EXTERN"},
    {static_cast<int>(Token::IDENTIFIER), "IDENTIFIER"},
    {static_cast<int>(Token::NUMBER), "NUMBER"},
};

static std::string identifier;
static double numVal;

static int GetToken() {
  static int lastChar = ' ';

  while (std::isspace(lastChar)) {
    lastChar = std::getchar();
  }

  if (std::isalpha(lastChar)) {
    identifier = lastChar;
    while (std::isalnum(lastChar = std::getchar())) {
      identifier += lastChar;
    }
    if (identifier == "def") {
      return static_cast<int>(Token::DEF);
    }
    if (identifier == "extern") {
      return static_cast<int>(Token::EXTERN);
    }
    return static_cast<int>(Token::IDENTIFIER);
  }

  if (std::isdigit(lastChar) || lastChar == '.') {
    std::string numStr;
    do {
      numStr += lastChar;
      lastChar = std::getchar();
    } while (std::isdigit(lastChar) || lastChar == '.');

    numVal = std::strtod(numStr.c_str(), nullptr);
    return static_cast<int>(Token::NUMBER);
  }

  if (lastChar == '#') {
    do {
      lastChar = std::getchar();
    } while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');

    if (lastChar != EOF) {
      return GetToken();
    }
  }

  if (lastChar == EOF) {
    return static_cast<int>(Token::ENOF);
  }
  int thisChar = lastChar;
  lastChar = std::getchar();
  return thisChar;
}