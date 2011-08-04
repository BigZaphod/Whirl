//--------------------------------------------
// WHIRL PROGRAMMING LANGUAGE LLVM COMPILER
// by: BigZaphod sean@fifthace.com
// http://www.bigzaphod.org/whirl/
// 
// License: Public Domain
// May 12, 2009
//--------------------------------------------
// 
// TO COMPILE:   g++ whirl-llvm.cpp `llvm-config --cxxflags --ldflags --libs core jit native` -o whirl
//

#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Module.h"
#include "llvm/ModuleProvider.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include <cstdio>
#include <string>
#include <map>
#include <vector>
using namespace llvm;

extern "C" {
  #include "whirl-runtime.c"
}

// a compiled whirl program is a global constant array of pointers to the two whirl instructions
// along with some global state for the current position of the wheels and instruction pointer

static const unsigned int kSizeOfMemory = 1024*4;

static Module *TheModule;
static IRBuilder<> Builder;
static FunctionPassManager *TheFPM;
static ExecutionEngine *TheExecutionEngine;

static Function *WhirlInstruction0;
static Function *WhirlInstruction1;
static Function *ExecuteUsingOperationsRing;
static Function *ExecuteUsingMathRing;
static Function *CompiledWhirlFunction;
static GlobalVariable *InstructionStream;       // array of function pointers
static GlobalVariable *InstructionPosition;     // 32 bit int
static GlobalVariable *OperationsRingPosition;  // 8 bit int
static GlobalVariable *OperationsRingDirection; // 1 bit int
static GlobalVariable *MathRingPosition;        // 8 bit int
static GlobalVariable *MathRingDirection;       // 1 bit int
static GlobalVariable *CurrentRing;             // 1 bit int
static GlobalVariable *ShouldExecuteCommand;    // 1 bit int
static GlobalVariable *MathValue;               // 32 bit int
static GlobalVariable *OperationsValue;         // 32 bit int
static GlobalVariable *Memory;                  // array of 32 bit ints
static GlobalVariable *MemoryPosition;          // 32 bit int
static Function *WhirlPrintIntFunction;
static Function *WhirlPrintASCIIFunction;
static Function *WhirlReadIntFunction;
static Function *WhirlReadASCIIFunction;

static unsigned int sizeOfWhirlProgram;

static FunctionType *WhirlFunctionType()
{
  std::vector<const Type*> Arguments;
  return FunctionType::get(Type::VoidTy, Arguments, false); 
}

static FunctionType *WhirlExternalFunctionType()
{
  std::vector<const Type*> Arguments(1,Type::Int32Ty);
  return FunctionType::get(Type::Int32Ty, Arguments, false);
}

static void BeginBuildingWhirlRuntime()
{
  FunctionType *FT = WhirlFunctionType();
  WhirlInstruction0 = Function::Create(FT, Function::InternalLinkage, "WhirlInstruction0", TheModule);
  WhirlInstruction1 = Function::Create(FT, Function::InternalLinkage, "WhirlInstruction1", TheModule);
  ExecuteUsingOperationsRing = Function::Create(FT, Function::InternalLinkage, "ExecuteUsingOperationsRing", TheModule);
  ExecuteUsingMathRing = Function::Create(FT, Function::InternalLinkage, "ExecuteUsingMathRing", TheModule);
  CompiledWhirlFunction = Function::Create(FT, Function::ExternalLinkage, "main", TheModule);
  WhirlPrintIntFunction = Function::Create(WhirlExternalFunctionType(), Function::ExternalLinkage, "WhirlPrintInt", TheModule);
  WhirlPrintASCIIFunction = Function::Create(WhirlExternalFunctionType(), Function::ExternalLinkage, "WhirlPrintASCII", TheModule);
  WhirlReadIntFunction = Function::Create(WhirlExternalFunctionType(), Function::ExternalLinkage, "WhirlReadInt", TheModule);
  WhirlReadASCIIFunction = Function::Create(WhirlExternalFunctionType(), Function::ExternalLinkage, "WhirlReadASCII", TheModule);
}

static void LoadWhirlInstructions(const char *filename)
{
  std::vector<Constant*> FunctionPointerValues;

  int byte;
  FILE *f = fopen(filename,"r");
  if (!f) {
    fprintf(stderr, "Could not open file: %s\n", filename);
    exit(-2);
  }
  for (int byte; (byte=getc(f), !feof(f));) {
    if (byte == '1' || byte == '0') {
      FunctionPointerValues.push_back((byte == '1')? WhirlInstruction1 : WhirlInstruction0);
    }
  }
  fclose(f);
  
  sizeOfWhirlProgram = FunctionPointerValues.size();
  Constant *FunctionPointers = ConstantArray::get(ArrayType::get(WhirlInstruction0->getType(),sizeOfWhirlProgram), FunctionPointerValues);  
  InstructionStream = new GlobalVariable(FunctionPointers->getType(), true, GlobalValue::InternalLinkage, FunctionPointers, "InstructionStream", TheModule);
}

static Value *CurrentMemoryValuePtr()
{
  std::vector<Value *> indexes;
  indexes.push_back(ConstantInt::get(Type::Int32Ty,0));
  indexes.push_back(Builder.CreateLoad(MemoryPosition));
  return Builder.CreateGEP(Memory,indexes.begin(),indexes.end());
}

#define DEBUG_VAL(XXXX)   Builder.CreateCall(WhirlPrintIntFunction,Builder.CreateZExt(XXXX,Type::Int32Ty));
#define DEBUG(XXXX)   Builder.CreateCall(WhirlPrintIntFunction,ConstantInt::getSigned(Type::Int32Ty,XXXX));

static void BuildWhirlInstruction0()
{
  // Create a new basic block to start insertion into.
  BasicBlock *entryBlock = BasicBlock::Create("entry", WhirlInstruction0);
  BasicBlock *mathBlock = BasicBlock::Create("math", WhirlInstruction0);
  BasicBlock *operationsBlock = BasicBlock::Create("operations", WhirlInstruction0);

  Builder.SetInsertPoint(entryBlock);
  Builder.CreateCondBr(Builder.CreateLoad(CurrentRing), mathBlock, operationsBlock);
  
  Builder.SetInsertPoint(mathBlock);
  Builder.CreateCall(ExecuteUsingMathRing);
  Builder.CreateRetVoid();

  Builder.SetInsertPoint(operationsBlock);
  Builder.CreateCall(ExecuteUsingOperationsRing);
  Builder.CreateRetVoid();

  verifyFunction(*WhirlInstruction0);
  TheFPM->run(*WhirlInstruction0);  // Optimize it.
}
        
static void BuildWhirlInstruction1()
{
  BasicBlock *entryBlock = BasicBlock::Create("entry", WhirlInstruction1);
  BasicBlock *selectOperationsRingBlock = BasicBlock::Create("selectOperationsRing", WhirlInstruction1);
  BasicBlock *selectMathRingBlock = BasicBlock::Create("selectMathRing", WhirlInstruction1);
  BasicBlock *doRotationBlock = BasicBlock::Create("doRotation", WhirlInstruction1);
  BasicBlock *rotateClockwiseBlock = BasicBlock::Create("rotateClockwise", WhirlInstruction1);
  BasicBlock *setPositionToZeroBlock = BasicBlock::Create("setPositionToZero", WhirlInstruction1);
  BasicBlock *rotateCounterclockwiseBlock = BasicBlock::Create("rotateCounterclockwise", WhirlInstruction1);
  BasicBlock *setPositionToElevenBlock = BasicBlock::Create("setPositionToEleven", WhirlInstruction1);
  BasicBlock *doneBlock = BasicBlock::Create("done", WhirlInstruction1);
  BasicBlock *updateOperationsRingBlock = BasicBlock::Create("updateOperationsRing", WhirlInstruction1);
  BasicBlock *updateMathRingBlock = BasicBlock::Create("updateMathRing", WhirlInstruction1);

  // the algo here is to check which ring is active, load the info for that into some local vars, do the rotation logic, then update the globals with the result
  
  Builder.SetInsertPoint(entryBlock);
  AllocaInst *allocaDirection = Builder.CreateAlloca(Type::Int1Ty);
  AllocaInst *allocaPosition = Builder.CreateAlloca(Type::Int8Ty);
  Builder.CreateStore(ConstantInt::get(Type::Int1Ty,0),ShouldExecuteCommand);
  Builder.CreateCondBr(Builder.CreateLoad(CurrentRing), selectMathRingBlock, selectOperationsRingBlock);
  
  Builder.SetInsertPoint(selectOperationsRingBlock);
  Builder.CreateStore(Builder.CreateLoad(OperationsRingDirection),allocaDirection);
  Builder.CreateStore(Builder.CreateLoad(OperationsRingPosition),allocaPosition);
  Builder.CreateBr(doRotationBlock);
  
  Builder.SetInsertPoint(selectMathRingBlock);
  Builder.CreateStore(Builder.CreateLoad(MathRingDirection),allocaDirection);
  Builder.CreateStore(Builder.CreateLoad(MathRingPosition),allocaPosition);
  Builder.CreateBr(doRotationBlock);

  Builder.SetInsertPoint(doRotationBlock);
  Builder.CreateCondBr(Builder.CreateLoad(allocaDirection), rotateCounterclockwiseBlock, rotateClockwiseBlock);

  Builder.SetInsertPoint(rotateClockwiseBlock);
  Builder.CreateStore(Builder.CreateAdd(Builder.CreateLoad(allocaPosition), ConstantInt::get(Type::Int8Ty,1)),allocaPosition);
  Builder.CreateCondBr(Builder.CreateICmpEQ(Builder.CreateLoad(allocaPosition),ConstantInt::get(Type::Int8Ty,12)),setPositionToZeroBlock,doneBlock);

  Builder.SetInsertPoint(setPositionToZeroBlock);
  Builder.CreateStore(ConstantInt::get(Type::Int8Ty,0),allocaPosition);
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(rotateCounterclockwiseBlock);
  Builder.CreateStore(Builder.CreateSub(Builder.CreateLoad(allocaPosition), ConstantInt::get(Type::Int8Ty,1)),allocaPosition);
  Builder.CreateCondBr(Builder.CreateICmpSLT(Builder.CreateLoad(allocaPosition),ConstantInt::get(Type::Int8Ty,0)),setPositionToElevenBlock,doneBlock);

  Builder.SetInsertPoint(setPositionToElevenBlock);
  Builder.CreateStore(ConstantInt::get(Type::Int8Ty,11),allocaPosition);
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(doneBlock);
  Builder.CreateCondBr(Builder.CreateLoad(CurrentRing), updateMathRingBlock, updateOperationsRingBlock);

  Builder.SetInsertPoint(updateOperationsRingBlock);
  Builder.CreateStore(Builder.CreateLoad(allocaPosition),OperationsRingPosition);
  Builder.CreateRetVoid();
  
  Builder.SetInsertPoint(updateMathRingBlock);
  Builder.CreateStore(Builder.CreateLoad(allocaPosition),MathRingPosition);
  Builder.CreateRetVoid();

  verifyFunction(*WhirlInstruction1);
  TheFPM->run(*WhirlInstruction1);  // Optimize it.
}

static void BuildMathRingFunction()
{
  BasicBlock *entryBlock = BasicBlock::Create("entry", ExecuteUsingMathRing);
  BasicBlock *executeBlock = BasicBlock::Create("execute", ExecuteUsingMathRing);
  BasicBlock *resetExecuteBlock = BasicBlock::Create("resetExecute", ExecuteUsingMathRing);
  BasicBlock *doneBlock = BasicBlock::Create("done", ExecuteUsingMathRing);

  BasicBlock *loadBlock = BasicBlock::Create("LOAD", ExecuteUsingMathRing);
  BasicBlock *storeBlock = BasicBlock::Create("STORE", ExecuteUsingMathRing);
  BasicBlock *addBlock = BasicBlock::Create("ADD", ExecuteUsingMathRing);
  BasicBlock *multBlock = BasicBlock::Create("MULT", ExecuteUsingMathRing);
  BasicBlock *divBlock = BasicBlock::Create("DIV", ExecuteUsingMathRing);
  BasicBlock *zeroBlock = BasicBlock::Create("ZERO", ExecuteUsingMathRing);
  BasicBlock *lessBlock = BasicBlock::Create("LESS", ExecuteUsingMathRing);
  BasicBlock *greatBlock = BasicBlock::Create("GREAT", ExecuteUsingMathRing);
  BasicBlock *equalBlock = BasicBlock::Create("EQUAL", ExecuteUsingMathRing);
  BasicBlock *notBlock = BasicBlock::Create("NOT", ExecuteUsingMathRing);
  BasicBlock *negBlock = BasicBlock::Create("NEG", ExecuteUsingMathRing);
  
  // always reverse the direction and check to see if we should even execute anything
  Builder.SetInsertPoint(entryBlock);
  Builder.CreateStore(Builder.CreateXor(Builder.CreateLoad(MathRingDirection), ConstantInt::get(Type::Int1Ty,1)),MathRingDirection);
  Builder.CreateCondBr(Builder.CreateLoad(ShouldExecuteCommand), executeBlock, resetExecuteBlock);
  
  Builder.SetInsertPoint(resetExecuteBlock);
  Builder.CreateStore(ConstantInt::get(Type::Int1Ty,1),ShouldExecuteCommand);
  Builder.CreateRetVoid();
  
  Builder.SetInsertPoint(executeBlock);
  SwitchInst *commands = Builder.CreateSwitch(Builder.CreateLoad(MathRingPosition),doneBlock,11);
  commands->addCase(ConstantInt::get(Type::Int8Ty,1), loadBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,2), storeBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,3), addBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,4), multBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,5), divBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,6), zeroBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,7), lessBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,8), greatBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,9), equalBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,10), notBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,11), negBlock);
  
  Builder.SetInsertPoint(loadBlock);
  Builder.CreateStore(Builder.CreateLoad(CurrentMemoryValuePtr()),MathValue);
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(storeBlock);
  Builder.CreateStore(Builder.CreateLoad(MathValue),CurrentMemoryValuePtr());
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(addBlock);
  Builder.CreateStore(Builder.CreateAdd(Builder.CreateLoad(MathValue),Builder.CreateLoad(CurrentMemoryValuePtr())),MathValue);
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(multBlock);
  Builder.CreateStore(Builder.CreateMul(Builder.CreateLoad(MathValue),Builder.CreateLoad(CurrentMemoryValuePtr())),MathValue);
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(divBlock);
  Builder.CreateStore(Builder.CreateSDiv(Builder.CreateLoad(MathValue),Builder.CreateLoad(CurrentMemoryValuePtr())),MathValue);
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(zeroBlock);
  Builder.CreateStore(ConstantInt::get(Type::Int32Ty,0), MathValue);
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(lessBlock);
  Builder.CreateStore(Builder.CreateZExt(Builder.CreateICmpSLT(Builder.CreateLoad(MathValue),Builder.CreateLoad(CurrentMemoryValuePtr())), Type::Int32Ty),MathValue);
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(greatBlock);
  Builder.CreateStore(Builder.CreateZExt(Builder.CreateICmpSGT(Builder.CreateLoad(MathValue),Builder.CreateLoad(CurrentMemoryValuePtr())), Type::Int32Ty),MathValue);
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(equalBlock);
  Builder.CreateStore(Builder.CreateZExt(Builder.CreateICmpEQ(Builder.CreateLoad(MathValue),Builder.CreateLoad(CurrentMemoryValuePtr())), Type::Int32Ty),MathValue);
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(notBlock);
  Builder.CreateStore(Builder.CreateZExt(Builder.CreateICmpEQ(ConstantInt::get(Type::Int32Ty,0),Builder.CreateLoad(MathValue)), Type::Int32Ty), MathValue);
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(negBlock);
  Builder.CreateStore(Builder.CreateMul(Builder.CreateLoad(MathValue), ConstantInt::getSigned(Type::Int32Ty,-1)), MathValue);
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(doneBlock);
  Builder.CreateStore(ConstantInt::get(Type::Int1Ty,0),ShouldExecuteCommand); 
  Builder.CreateStore(ConstantInt::get(Type::Int1Ty,0),CurrentRing);
  Builder.CreateRetVoid();
  verifyFunction(*ExecuteUsingMathRing);
  TheFPM->run(*ExecuteUsingMathRing); // Optimize it.
}

static void BuildOperationsRingFunction()
{
  BasicBlock *entryBlock = BasicBlock::Create("entry", ExecuteUsingOperationsRing);
  BasicBlock *executeBlock = BasicBlock::Create("execute", ExecuteUsingOperationsRing);
  BasicBlock *resetExecuteBlock = BasicBlock::Create("resetExecute", ExecuteUsingOperationsRing);
  BasicBlock *doneBlock = BasicBlock::Create("done", ExecuteUsingOperationsRing);

  BasicBlock *exitBlock = BasicBlock::Create("EXIT", ExecuteUsingOperationsRing);
  BasicBlock *oneBlock = BasicBlock::Create("ONE", ExecuteUsingOperationsRing);
  BasicBlock *zeroBlock = BasicBlock::Create("ZERO", ExecuteUsingOperationsRing);
  BasicBlock *loadBlock = BasicBlock::Create("LOAD", ExecuteUsingOperationsRing);
  BasicBlock *storeBlock = BasicBlock::Create("STORE", ExecuteUsingOperationsRing);
  BasicBlock *paddBlock = BasicBlock::Create("PADD", ExecuteUsingOperationsRing);
  BasicBlock *daddBlock = BasicBlock::Create("DADD", ExecuteUsingOperationsRing);
  BasicBlock *logicBlock = BasicBlock::Create("LOGIC", ExecuteUsingOperationsRing);
  BasicBlock *ifBlock = BasicBlock::Create("IF", ExecuteUsingOperationsRing);
  BasicBlock *intIOBlock = BasicBlock::Create("INTIO", ExecuteUsingOperationsRing);
  BasicBlock *ascIOBlock = BasicBlock::Create("ASCIO", ExecuteUsingOperationsRing);
  BasicBlock *readIntBlock = BasicBlock::Create("readInt", ExecuteUsingOperationsRing);
  BasicBlock *printIntBlock = BasicBlock::Create("printInt", ExecuteUsingOperationsRing);
  BasicBlock *readASCIIBlock = BasicBlock::Create("readASCII", ExecuteUsingOperationsRing);
  BasicBlock *printASCIIBlock = BasicBlock::Create("printASCII", ExecuteUsingOperationsRing);
  BasicBlock *logicTestOkayBlock = BasicBlock::Create("logicTestOkay", ExecuteUsingOperationsRing);
  
  // always reverse the direction and check to see if we should even execute anything
  Builder.SetInsertPoint(entryBlock);
  Builder.CreateStore(Builder.CreateXor(Builder.CreateLoad(OperationsRingDirection), ConstantInt::get(Type::Int1Ty,1)),OperationsRingDirection);
  Builder.CreateCondBr(Builder.CreateLoad(ShouldExecuteCommand), executeBlock, resetExecuteBlock);
  
  Builder.SetInsertPoint(resetExecuteBlock);
  Builder.CreateStore(ConstantInt::get(Type::Int1Ty,1),ShouldExecuteCommand);
  Builder.CreateRetVoid();
  
  Builder.SetInsertPoint(executeBlock);
  SwitchInst *commands = Builder.CreateSwitch(Builder.CreateLoad(OperationsRingPosition),doneBlock,11);
  commands->addCase(ConstantInt::get(Type::Int8Ty,1), exitBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,2), oneBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,3), zeroBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,4), loadBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,5), storeBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,6), paddBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,7), daddBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,8), logicBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,9), ifBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,10), intIOBlock);
  commands->addCase(ConstantInt::get(Type::Int8Ty,11), ascIOBlock);
  
  Builder.SetInsertPoint(exitBlock);
  Builder.CreateStore(ConstantInt::get(Type::Int32Ty,sizeOfWhirlProgram),InstructionPosition);
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(oneBlock);
  Builder.CreateStore(ConstantInt::get(Type::Int32Ty,1),OperationsValue);
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(zeroBlock);
  Builder.CreateStore(ConstantInt::get(Type::Int32Ty,0),OperationsValue);
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(loadBlock);
  Builder.CreateStore(Builder.CreateLoad(CurrentMemoryValuePtr()),OperationsValue);
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(storeBlock);
  Builder.CreateStore(Builder.CreateLoad(OperationsValue), CurrentMemoryValuePtr());
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(paddBlock);
  // note that is subtracts one from the value because when we get back to the main run loop, it always adds one to the instruction position - so this should get us where we want
  Builder.CreateStore(Builder.CreateAdd(Builder.CreateLoad(InstructionPosition),Builder.CreateSub(Builder.CreateLoad(OperationsValue),ConstantInt::get(Type::Int32Ty,1))), InstructionPosition);
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(daddBlock);
  Builder.CreateStore(Builder.CreateAdd(Builder.CreateLoad(MemoryPosition),Builder.CreateLoad(OperationsValue)), MemoryPosition);
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(logicBlock);
  Builder.CreateCondBr(Builder.CreateICmpNE(ConstantInt::get(Type::Int32Ty,0),Builder.CreateLoad(CurrentMemoryValuePtr())),logicTestOkayBlock,zeroBlock);
  
  Builder.SetInsertPoint(logicTestOkayBlock);
  Builder.CreateStore(Builder.CreateZExt(Builder.CreateICmpNE(ConstantInt::get(Type::Int32Ty,0),Builder.CreateLoad(OperationsValue)),Type::Int32Ty), OperationsValue);
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(ifBlock);
  Builder.CreateCondBr(Builder.CreateICmpNE(ConstantInt::get(Type::Int32Ty,0),Builder.CreateLoad(OperationsValue)),paddBlock,doneBlock);
  
  Builder.SetInsertPoint(intIOBlock);
  Builder.CreateCondBr(Builder.CreateICmpEQ(ConstantInt::get(Type::Int32Ty,0),Builder.CreateLoad(OperationsValue)),readIntBlock,printIntBlock);

  Builder.SetInsertPoint(readIntBlock);
  Builder.CreateStore(Builder.CreateCall(WhirlReadIntFunction,ConstantInt::get(Type::Int32Ty,0)),CurrentMemoryValuePtr());
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(printIntBlock);
  Builder.CreateCall(WhirlPrintIntFunction,Builder.CreateLoad(CurrentMemoryValuePtr()));
  Builder.CreateBr(doneBlock);

  Builder.SetInsertPoint(ascIOBlock);
  Builder.CreateCondBr(Builder.CreateICmpEQ(ConstantInt::get(Type::Int32Ty,0),Builder.CreateLoad(OperationsValue)),readASCIIBlock,printASCIIBlock);

  Builder.SetInsertPoint(readASCIIBlock);
  Builder.CreateStore(Builder.CreateCall(WhirlReadASCIIFunction,ConstantInt::get(Type::Int32Ty,0)),CurrentMemoryValuePtr());
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(printASCIIBlock);
  Builder.CreateCall(WhirlPrintASCIIFunction,Builder.CreateLoad(CurrentMemoryValuePtr()));
  Builder.CreateBr(doneBlock);
  
  Builder.SetInsertPoint(doneBlock);
  Builder.CreateStore(ConstantInt::get(Type::Int1Ty,0),ShouldExecuteCommand); 
  Builder.CreateStore(ConstantInt::get(Type::Int1Ty,1),CurrentRing);
  Builder.CreateRetVoid();
  verifyFunction(*ExecuteUsingOperationsRing);
  TheFPM->run(*ExecuteUsingOperationsRing); // Optimize it.
}

static void FinishBuildingWhirlRuntime()
{
  InstructionPosition = new GlobalVariable(Type::Int32Ty, false, GlobalValue::InternalLinkage, ConstantInt::get(Type::Int32Ty,0), "InstructionPosition", TheModule);
  OperationsRingPosition = new GlobalVariable(Type::Int8Ty, false, GlobalValue::InternalLinkage, ConstantInt::get(Type::Int8Ty,0), "OperationsRingPosition", TheModule);
  OperationsRingDirection = new GlobalVariable(Type::Int1Ty, false, GlobalValue::InternalLinkage, ConstantInt::get(Type::Int1Ty,0), "OperationsRingDirection", TheModule);
  MathRingPosition = new GlobalVariable(Type::Int8Ty, false, GlobalValue::InternalLinkage, ConstantInt::get(Type::Int8Ty,0), "MathRingPosition", TheModule);
  MathRingDirection = new GlobalVariable(Type::Int1Ty, false, GlobalValue::InternalLinkage, ConstantInt::get(Type::Int1Ty,0), "MathRingDirection", TheModule);
  CurrentRing = new GlobalVariable(Type::Int1Ty, false, GlobalValue::InternalLinkage, ConstantInt::get(Type::Int1Ty,0), "CurrentRing", TheModule);
  ShouldExecuteCommand = new GlobalVariable(Type::Int1Ty, false, GlobalValue::InternalLinkage, ConstantInt::get(Type::Int1Ty,0), "ShouldExecuteCommand", TheModule);
  MathValue = new GlobalVariable(Type::Int32Ty, false, GlobalValue::InternalLinkage, ConstantInt::get(Type::Int32Ty,0), "MathValue", TheModule);
  OperationsValue = new GlobalVariable(Type::Int32Ty, false, GlobalValue::InternalLinkage, ConstantInt::get(Type::Int32Ty,0), "OperationsValue", TheModule);
  
  // Make the memory - and make it all init to 0.
  std::vector<Constant*> ZeroedMemory(kSizeOfMemory,ConstantInt::get(Type::Int32Ty,0));
  Memory = new GlobalVariable(ArrayType::get(Type::Int32Ty,kSizeOfMemory), false, GlobalValue::InternalLinkage, ConstantArray::get(ArrayType::get(Type::Int32Ty,kSizeOfMemory), ZeroedMemory), "Memory", TheModule);
  MemoryPosition = new GlobalVariable(Type::Int32Ty, false, GlobalValue::InternalLinkage, ConstantInt::get(Type::Int32Ty,0), "MemoryPosition", TheModule);
  
  BuildWhirlInstruction0();
  BuildWhirlInstruction1();
  BuildMathRingFunction();
  BuildOperationsRingFunction();
}

static void CompileWhirl()
{
  // Create a new basic block to start insertion into.
  BasicBlock *entryBlock = BasicBlock::Create("entry", CompiledWhirlFunction);
  BasicBlock *mainBlock = BasicBlock::Create("main", CompiledWhirlFunction);
  BasicBlock *executeBlock = BasicBlock::Create("execute", CompiledWhirlFunction);
  BasicBlock *exitBlock = BasicBlock::Create("exit", CompiledWhirlFunction);

  Builder.SetInsertPoint(entryBlock);
  Builder.CreateBr(mainBlock);
  
  // fetch the current instruction from the program array, then run it
  Builder.SetInsertPoint(mainBlock);
  Builder.CreateCondBr(Builder.CreateICmpULT(Builder.CreateLoad(InstructionPosition), ConstantInt::get(Type::Int32Ty,sizeOfWhirlProgram)), executeBlock, exitBlock);
  
  Builder.SetInsertPoint(executeBlock);
  std::vector<Value *> indexes;
  indexes.push_back(ConstantInt::get(Type::Int32Ty,0));
  indexes.push_back(Builder.CreateLoad(InstructionPosition));
  Builder.CreateCall(Builder.CreateLoad(Builder.CreateGEP(InstructionStream,indexes.begin(),indexes.end())));
  Builder.CreateStore( Builder.CreateAdd(Builder.CreateLoad(InstructionPosition),ConstantInt::get(Type::Int32Ty,1)), InstructionPosition);  
  Builder.CreateBr(mainBlock);

  Builder.SetInsertPoint(exitBlock);
  Builder.CreateRetVoid();
  
  verifyFunction(*CompiledWhirlFunction);
  TheFPM->run(*CompiledWhirlFunction);  // Optimize it.
}

int main(int argc, char *argv[])
{
  if (argc < 2) {
    fprintf(stderr,"Usage: %s <whirl source filename>\n", argv[0]);
    exit(-1);
  }
  
  // Make the module, which holds all the code.
  TheModule = new Module("whirl jit");
  
  // Create the JIT.
  TheExecutionEngine = ExecutionEngine::create(TheModule);

  {
    ExistingModuleProvider OurModuleProvider(TheModule);
    FunctionPassManager OurFPM(&OurModuleProvider);

    // Set up the optimizer pipeline.  Start with registering info about how the
    // target lays out data structures.
    OurFPM.add(new TargetData(*TheExecutionEngine->getTargetData()));
    // Promote allocas to registers.
    OurFPM.add(createPromoteMemoryToRegisterPass());
    // Do simple "peephole" optimizations and bit-twiddling optzns.
    OurFPM.add(createInstructionCombiningPass());
    // Reassociate expressions.
    OurFPM.add(createReassociatePass());
    // Eliminate Common SubExpressions.
    OurFPM.add(createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    OurFPM.add(createCFGSimplificationPass());

    // Set the global so the code gen can use this.
    TheFPM = &OurFPM;

    // Configure and run the main compiler now.
    BeginBuildingWhirlRuntime();
    LoadWhirlInstructions(argv[1]);
    FinishBuildingWhirlRuntime();
    CompileWhirl();

    TheFPM = 0;

    // Print out all of the generated code.
    TheModule->dump();

    // Now JIT the code and run it.
    void *FPtr = TheExecutionEngine->getPointerToFunction(CompiledWhirlFunction);

    // Cast it to the right type (takes no arguments, returns void) so we can call the compiled Whirl program as a native function.
    void (*FP)() = (void (*)())FPtr;
    FP();

  }  // Free module provider (and thus the module) and pass manager.
  
  return 0;
}
