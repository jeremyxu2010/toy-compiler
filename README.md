# 写一个编译器

## 环境准备

首先安装`flex`、`bison`、`llvm`，分别用来进行词法分析、语法分析、构造AST及生成LLVM IR

```bash
$ brew install flex
$ brew install bison
$ brew install llvm
$ export PATH="/usr/local/opt/flex/bin:$PATH"
$ export PATH="/usr/local/opt/bison/bin:$PATH"
$ export PATH="/usr/local/opt/llvm/bin:$PATH"
```

## 定义词法token

`tokens.l`

```cpp
%{
#include <string>
#include "node.h"
#include "parser.hpp"

#define SAVE_TOKEN  yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t)    (yylval.token = t)
%}

%option noyywrap

%%

[ \t\n]                                         ;
"extern"                                        return TOKEN(TEXTERN);
"return"                                        return TOKEN(TRETURN);
[a-zA-Z_][a-zA-Z0-9_]*                          SAVE_TOKEN; return TIDENTIFIER;
[0-9]+\.[0-9]*                                  SAVE_TOKEN; return TDOUBLE;
[0-9]+                                          SAVE_TOKEN; return TINTEGER;

"="                                             return TOKEN(TEQUAL);
"=="                                            return TOKEN(TCEQ);
"!="                                            return TOKEN(TCNE);
"<"                                             return TOKEN(TCLT);
"<="                                            return TOKEN(TCLE);
">"                                             return TOKEN(TCGT);
">="                                            return TOKEN(TCGE);

"("                                             return TOKEN(TLPAREN);
")"                                             return TOKEN(TRPAREN);
"{"                                             return TOKEN(TLBRACE);
"}"                                             return TOKEN(TRBRACE);

"."                                             return TOKEN(TDOT);
","                                             return TOKEN(TCOMMA);

"+"                                             return TOKEN(TPLUS);
"-"                                             return TOKEN(TMINUS);
"*"                                             return TOKEN(TMUL);
"/"                                             return TOKEN(TDIV);

.                                               printf("Unknown token!\n"); yyterminate();

%%

```

## 定义AST中树节点的元素

`node.h`

```cpp
#include <iostream>
#include <vector>
#include <llvm/IR/Value.h>

class CodeGenContext;
class NStatement;
class NExpression;
class NVariableDeclaration;

typedef std::vector<NStatement*> StatementList;
typedef std::vector<NExpression*> ExpressionList;
typedef std::vector<NVariableDeclaration*> VariableList;

class Node {
public:
        virtual ~Node() {}
        virtual llvm::Value* codeGen(CodeGenContext& context) { return NULL; }
};

class NExpression : public Node {
};

class NStatement : public Node {
};

class NInteger : public NExpression {
public:
        long long value;
        NInteger(long long value) : value(value) { }
        virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NDouble : public NExpression {
public:
        double value;
        NDouble(double value) : value(value) { }
        virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NIdentifier : public NExpression {
public:
        std::string name;
        NIdentifier(const std::string& name) : name(name) { }
        virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NMethodCall : public NExpression {
public:
        const NIdentifier& id;
        ExpressionList arguments;
        NMethodCall(const NIdentifier& id, ExpressionList& arguments) :
                id(id), arguments(arguments) { }
        NMethodCall(const NIdentifier& id) : id(id) { }
        virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBinaryOperator : public NExpression {
public:
        int op;
        NExpression& lhs;
        NExpression& rhs;
        NBinaryOperator(NExpression& lhs, int op, NExpression& rhs) :
                lhs(lhs), rhs(rhs), op(op) { }
        virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NAssignment : public NExpression {
public:
        NIdentifier& lhs;
        NExpression& rhs;
        NAssignment(NIdentifier& lhs, NExpression& rhs) : 
                lhs(lhs), rhs(rhs) { }
        virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NBlock : public NExpression {
public:
        StatementList statements;
        NBlock() { }
        virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExpressionStatement : public NStatement {
public:
        NExpression& expression;
        NExpressionStatement(NExpression& expression) : 
                expression(expression) { }
        virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NReturnStatement : public NStatement {
public:
        NExpression& expression;
        NReturnStatement(NExpression& expression) : 
                expression(expression) { }
        virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NVariableDeclaration : public NStatement {
public:
        const NIdentifier& type;
        NIdentifier& id;
        NExpression *assignmentExpr;
        NVariableDeclaration(const NIdentifier& type, NIdentifier& id) :
                type(type), id(id) { assignmentExpr = NULL; }
        NVariableDeclaration(const NIdentifier& type, NIdentifier& id, NExpression *assignmentExpr) :
                type(type), id(id), assignmentExpr(assignmentExpr) { }
        virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NExternDeclaration : public NStatement {
public:
    const NIdentifier& type;
    const NIdentifier& id;
    VariableList arguments;
    NExternDeclaration(const NIdentifier& type, const NIdentifier& id,
            const VariableList& arguments) :
        type(type), id(id), arguments(arguments) {}
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class NFunctionDeclaration : public NStatement {
public:
        const NIdentifier& type;
        const NIdentifier& id;
        VariableList arguments;
        NBlock& block;
        NFunctionDeclaration(const NIdentifier& type, const NIdentifier& id, 
                        const VariableList& arguments, NBlock& block) :
                type(type), id(id), arguments(arguments), block(block) { }
        virtual llvm::Value* codeGen(CodeGenContext& context);
};

```

## 定义语法结构

`parser.y`

```cpp
%{
        #include "node.h"
        #include <cstdio>
        #include <cstdlib>
        NBlock *programBlock; /* the top level root node of our final AST */

        extern int yylex();
        void yyerror(const char *s) { std::printf("Error: %s\n", s);std::exit(1); }
%}

/* Represents the many different ways we can access our data */
%union {
        Node *node;
        NBlock *block;
        NExpression *expr;
        NStatement *stmt;
        NIdentifier *ident;
        NVariableDeclaration *var_decl;
        std::vector<NVariableDeclaration*> *varvec;
        std::vector<NExpression*> *exprvec;
        std::string *string;
        int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> TIDENTIFIER TINTEGER TDOUBLE
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL
%token <token> TLPAREN TRPAREN TLBRACE TRBRACE TCOMMA TDOT
%token <token> TPLUS TMINUS TMUL TDIV
%token <token> TRETURN TEXTERN

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> numeric expr_call expr_value expr_assign
%type <varvec> func_decl_args
%type <exprvec> call_args
%type <block> program stmts block
%type <stmt> stmt var_decl func_decl extern_decl
%type <token> comparison calculation

/* Operator precedence for mathematical operators */
%left TMUL TDIV TPLUS TMINUS TCEQ TCNE TCLT TCLE TCGT TCGE

%start program

%%

program : stmts { programBlock = $1; }
          ;
                
stmts : stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
          | stmts stmt { $1->statements.push_back($<stmt>2); }
          ;

stmt : func_decl
          | var_decl
          | extern_decl
          | expr_call { $$ = new NExpressionStatement(*$1); }
          | expr_assign { $$ = new NExpressionStatement(*$1); }
          | TRETURN expr_call { $$ = new NReturnStatement(*$2); }
          | TRETURN expr_value { $$ = new NReturnStatement(*$2); }
          ;

block : TLBRACE stmts TRBRACE { $$ = $2; }
          | TLBRACE TRBRACE { $$ = new NBlock(); }
          ;

var_decl : ident ident { $$ = new NVariableDeclaration(*$1, *$2); }
          | ident ident TEQUAL expr_call { $$ = new NVariableDeclaration(*$1, *$2, $4); }
          | ident ident TEQUAL expr_value { $$ = new NVariableDeclaration(*$1, *$2, $4); }
          ;

extern_decl : TEXTERN ident ident TLPAREN func_decl_args TRPAREN
                { $$ = new NExternDeclaration(*$2, *$3, *$5); delete $5; }
          ;

func_decl : ident ident TLPAREN func_decl_args TRPAREN block 
                { $$ = new NFunctionDeclaration(*$1, *$2, *$4, *$6); delete $4; }
          ;
        
func_decl_args : /*blank*/  { $$ = new VariableList(); }
          | var_decl { $$ = new VariableList(); $$->push_back($<var_decl>1); }
          | func_decl_args TCOMMA var_decl { $1->push_back($<var_decl>3); }
          ;

ident : TIDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
          ;

numeric : TINTEGER { $$ = new NInteger(atol($1->c_str())); delete $1; }
          | TDOUBLE { $$ = new NDouble(atof($1->c_str())); delete $1; }
          ;

expr_assign : ident TEQUAL expr_call { $$ = new NAssignment(*$<ident>1, *$3); }
          | ident TEQUAL expr_value { $$ = new NAssignment(*$<ident>1, *$3); }
          ;

expr_call : ident TLPAREN call_args TRPAREN { $$ = new NMethodCall(*$1, *$3); delete $3; }
          ;

expr_value: ident { $<ident>$ = $1; }
          | numeric
          | TLPAREN expr_value TRPAREN { $$ = $2; }
          | expr_value calculation expr_value %prec TMUL { $$ = new NBinaryOperator(*$1, $2, *$3); }
          | expr_value comparison expr_value %prec TCEQ { $$ = new NBinaryOperator(*$1, $2, *$3); }
          ;
        
call_args : /*blank*/  { $$ = new ExpressionList(); }
          | expr_value { $$ = new ExpressionList(); $$->push_back($1); }
          | expr_call { $$ = new ExpressionList(); $$->push_back($1); }
          | call_args TCOMMA expr_value  { $1->push_back($3); }
          | call_args TCOMMA expr_call  { $1->push_back($3); }
          ;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE;

calculation: TMUL | TDIV | TPLUS | TMINUS;

%%

```

## 定义AST中每类树节点生成的LLVM IR

`codegen.h`

```cpp
#include <stack>
#include <typeinfo>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Bitcode/BitcodeWriter.h>

using namespace llvm;

class NBlock;

static LLVMContext MyContext;

class CodeGenBlock {
public:
    BasicBlock *block;
    Value *returnValue;
    std::map<std::string, Value*> locals;
};

class CodeGenContext {
    std::stack<CodeGenBlock *> blocks;
    Function *mainFunction;

public:

    Module *module;
    CodeGenContext() { module = new Module("main", MyContext); }
    
    void generateCode(NBlock& root, std::string bcFile);
    GenericValue runCode();
    std::map<std::string, Value*>& locals() { return blocks.top()->locals; }
    BasicBlock *currentBlock() { return blocks.top()->block; }
    void pushBlock(BasicBlock *block) { blocks.push(new CodeGenBlock()); blocks.top()->returnValue = NULL; blocks.top()->block = block; }
    void popBlock() { CodeGenBlock *top = blocks.top(); blocks.pop(); delete top; }
    void setCurrentReturnValue(Value *value) { blocks.top()->returnValue = value; }
    Value* getCurrentReturnValue() { return blocks.top()->returnValue; }
};
```

`codegen.cpp`

```cpp
#include "node.h"
#include "codegen.h"
#include "parser.hpp"

using namespace std;

/* Compile the AST into a module */
void CodeGenContext::generateCode(NBlock& root, std::string bcFile)
{
        std::cout << "Generating code...\n";
        
        /* Create the top level interpreter function to call as entry */
        vector<Type*> argTypes;
        FunctionType *ftype = FunctionType::get(Type::getInt32Ty(MyContext), makeArrayRef(argTypes), false);
        mainFunction = Function::Create(ftype, GlobalValue::ExternalLinkage, "main", module);
        BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", mainFunction, 0);
        
        /* Push a new variable/block context */
        pushBlock(bblock);
        root.codeGen(*this); /* emit bytecode for the toplevel block */
        ReturnInst::Create(MyContext, ConstantInt::get(Type::getInt32Ty(MyContext), 0), bblock);
        popBlock();
        
        /* Print the bytecode in a human-readable format 
           to see if our program compiled properly
         */
        std::cout << "Code is generated.\n";

        legacy::PassManager pm;
        pm.add(createPrintModulePass(outs()));
        pm.run(*module);

        std::error_code errInfo;
        llvm::raw_ostream *out = new llvm::raw_fd_ostream(bcFile, errInfo, sys::fs::F_None);
        llvm::WriteBitcodeToFile(*module, *out);
        out->flush();
        delete out;
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
        std::cout << "Running code...\n";
        ExecutionEngine *ee = EngineBuilder( unique_ptr<Module>(module) ).create();
        ee->finalizeObject();
        vector<GenericValue> noargs;
        GenericValue v = ee->runFunction(mainFunction, noargs);
        std::cout << "Code was run.\n";
        return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const NIdentifier& type) 
{
        if (type.name.compare("int") == 0) {
                return Type::getInt64Ty(MyContext);
        }
        else if (type.name.compare("double") == 0) {
                return Type::getDoubleTy(MyContext);
        }
        return Type::getVoidTy(MyContext);
}

/* -- Code Generation -- */

Value* NInteger::codeGen(CodeGenContext& context)
{
        std::cout << "Creating integer: " << value << endl;
        return ConstantInt::get(Type::getInt64Ty(MyContext), value, true);
}

Value* NDouble::codeGen(CodeGenContext& context)
{
        std::cout << "Creating double: " << value << endl;
        return ConstantFP::get(Type::getDoubleTy(MyContext), value);
}

Value* NIdentifier::codeGen(CodeGenContext& context)
{
        std::cout << "Creating identifier reference: " << name << endl;
        if (context.locals().find(name) == context.locals().end()) {
                std::cerr << "undeclared variable " << name << endl;
                return NULL;
        }
        return new LoadInst(context.locals()[name], "", false, context.currentBlock());
}

Value* NMethodCall::codeGen(CodeGenContext& context)
{
        Function *function = context.module->getFunction(id.name.c_str());
        if (function == NULL) {
                std::cerr << "no such function " << id.name << endl;
        }
        std::vector<Value*> args;
        ExpressionList::const_iterator it;
        for (it = arguments.begin(); it != arguments.end(); it++) {
                args.push_back((**it).codeGen(context));
        }
        CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
        std::cout << "Creating method call: " << id.name << endl;
        return call;
}

Value* NBinaryOperator::codeGen(CodeGenContext& context)
{
        std::cout << "Creating binary operation " << op << endl;
        Instruction::BinaryOps instr;
        switch (op) {
                case TPLUS: 	instr = Instruction::Add; goto math;
                case TMINUS: 	instr = Instruction::Sub; goto math;
                case TMUL: 	instr = Instruction::Mul; goto math;
                case TDIV: 	instr = Instruction::SDiv; goto math;
                                
                /* TODO comparison */
        }

        return NULL;
math:
        return BinaryOperator::Create(instr, lhs.codeGen(context), 
                rhs.codeGen(context), "", context.currentBlock());
}

Value* NAssignment::codeGen(CodeGenContext& context)
{
        std::cout << "Creating assignment for " << lhs.name << endl;
        if (context.locals().find(lhs.name) == context.locals().end()) {
                std::cerr << "undeclared variable " << lhs.name << endl;
                return NULL;
        }
        return new StoreInst(rhs.codeGen(context), context.locals()[lhs.name], false, context.currentBlock());
}

Value* NBlock::codeGen(CodeGenContext& context)
{
        StatementList::const_iterator it;
        Value *last = NULL;
        for (it = statements.begin(); it != statements.end(); it++) {
                std::cout << "Generating code for " << typeid(**it).name() << endl;
                last = (**it).codeGen(context);
        }
        std::cout << "Creating block" << endl;
        return last;
}

Value* NExpressionStatement::codeGen(CodeGenContext& context)
{
        std::cout << "Generating code for " << typeid(expression).name() << endl;
        return expression.codeGen(context);
}

Value* NReturnStatement::codeGen(CodeGenContext& context)
{
        std::cout << "Generating return code for " << typeid(expression).name() << endl;
        Value *returnValue = expression.codeGen(context);
        context.setCurrentReturnValue(returnValue);
        return returnValue;
}

Value* NVariableDeclaration::codeGen(CodeGenContext& context)
{
        std::cout << "Creating variable declaration " << type.name << " " << id.name << endl;
        AllocaInst *alloc = new AllocaInst(typeOf(type), NULL, id.name.c_str(), context.currentBlock());
        context.locals()[id.name] = alloc;
        if (assignmentExpr != NULL) {
                NAssignment assn(id, *assignmentExpr);
                assn.codeGen(context);
        }
        return alloc;
}

Value* NExternDeclaration::codeGen(CodeGenContext& context)
{
    vector<Type*> argTypes;
    VariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(typeOf((**it).type));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);
    return function;
}

Value* NFunctionDeclaration::codeGen(CodeGenContext& context)
{
        vector<Type*> argTypes;
        VariableList::const_iterator it;
        for (it = arguments.begin(); it != arguments.end(); it++) {
                argTypes.push_back(typeOf((**it).type));
        }
        FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
        Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, id.name.c_str(), context.module);
        BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", function, 0);

        context.pushBlock(bblock);

        Function::arg_iterator argsValues = function->arg_begin();
        Value* argumentValue;

        for (it = arguments.begin(); it != arguments.end(); it++) {
                (**it).codeGen(context);
                
                argumentValue = &*argsValues++;
                argumentValue->setName((*it)->id.name.c_str());
                StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->id.name], false, bblock);
        }
        
        block.codeGen(context);
        ReturnInst::Create(MyContext, context.getCurrentReturnValue(), bblock);

        context.popBlock();
        std::cout << "Creating function: " << id.name << endl;
        return function;
}

```

## 使用写的编译器将源码编译为二进制文件

`main.cpp`

```cpp
#include <iostream>
#include "codegen.h"
#include "node.h"

using namespace std;

extern FILE *yyin;
extern int yyparse();
extern NBlock *programBlock;

void createCoreFunctions(CodeGenContext &context);

int main(int argc, char **argv)
{
        FILE *fp = fopen("test/example.txt", "r");
        if (!fp)
        {
                printf("couldn't open file for reading\n");
                exit(-1);
        }
        yyin = fp;
        int parseErr = yyparse();
        if (parseErr != 0) {
                printf("couldn't complete lex parse\n");
                exit(-1);
        }
        fclose(fp);
        InitializeNativeTarget();
        InitializeNativeTargetAsmPrinter();
        InitializeNativeTargetAsmParser();
        CodeGenContext context;
        createCoreFunctions(context);
        context.generateCode(*programBlock, "test/example.bc");
        return 0;
}
```

执行命令：

```bash
$ make compile # compile source file
$ ./test/example.native # execute binary file
```

## 参考
1. http://gnuu.org/2009/09/18/writing-your-own-toy-compiler

