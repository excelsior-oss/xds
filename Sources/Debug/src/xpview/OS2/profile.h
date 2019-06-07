//
// DEFINITION MODULE Profile;
//

#ifndef __PROFILE_H_
#define __PROFILE_H_
#include <os2.h>

#define IDENTKEY "XDS Profiler";
#define VERSION  "0.03 beta";

typedef PSZ      NAME;

extern "C"
{

  //-- LoadDebugInfo return code

  #define RC_Error                        0 //-- Error
  #define RC_OpenErrorProfilerData       -1 //-- Error opening profiler trace file
  #define RC_ReadErrorProfilerData       -2 //-- Error reading profiler trace file
  #define RC_ReadDebugInfo               -3 //-- Error reading debugging information
  #define RC_WrongFormatProfilerData     -4 //-- Wrong format of profiler trace file
  #define RC_IsNot_XDS_ProfilerTraceFile -5 //-- Is not XDS profiler trace file


  //-- Load profiler trace file and debugging information.
  //-- "name" is name of profiler trace file
  //-- Result:
  //--   <= 0 in case of error (see above)
  //--   >  0 if success (= number of components)
  int _System LoadDebugInfo (PSZ name);


  //-- Clear debug information
  void _System ClearDebugInfo ();


  //-- Number of snapshots
  //-- "Snapshots" is a total number of EIP values collected during execution trace.
  int _System GetSnapshots ();


  //-- Component name
  //-- nCom from [0..Quantity_Components-1]
  NAME _System ComponentName (int nCom);

  //-- Number of modules or publics in component
  //-- nCom from [0..Quantity_Components-1]
  //-- result = 0 - no parts in components
  //-- result > 0 - quantity modules
  //-- result < 0 - quantity publics
  int _System N_Parts (int nCom);

  //-- Component shapshots
  //-- nCom from [0..Quantity_Components-1]
  int _System ComponentSnapshots (int nCom);

  //-- Quantity of snapshots in unknown parts
  int _System GetUnknownParts (int nCom);


  //-- Public name
  //-- nPublic from [0..Quantity_Public-1]
  NAME _System PublicName (int nCom, int nPublic);

  //-- Public shapshots
  //-- nPublic from [0..Quantity_Publics-1]
  int _System PublicSnapshots (int nCom, int nPublic);

  //-- Public attributes: address and lenght
  //-- nPublic from [0..Quantity_Publics-1]
  void _System PublicAttr (int nCom, int nPublic, PULONG addr, PULONG length);


  //-- Module name
  //-- nModule from [0..Quantity_Modules-1]
  NAME _System ModuleName (int nCom, int nModule);

  //-- Source file name
  //-- nModule from [0..Quantity_Modules-1]
  NAME _System SourceName (int nCom, int nModule);

  //-- Module shapshots
  //-- nModule from [0..Quantity_Modules-1]
  int _System ModuleSnapshots (int nCom, int nModule);


  //-- Line snapshots
  //-- nModule from [0..Quantity_Modules-1]
  //-- nLine from [0..Quantity_Lines-1]
  int _System LineSnapshots (int nCom, int nModule, int nLine);


  //-- Number of procedures in module
  //-- nModule from [0..Quantity_Modules-1]
  int _System N_Proc (int nCom, int nModule);


  //-- Procedure name
  //-- nModule from [0..Quantity_Modules-1]
  //-- nProc from [0..Quantity_Procedures-1]
  NAME _System ProcName (int nCom, int nModule, int nProc);


  //-- Procedure snapshots
  //-- nModule from [0..Quantity_Modules-1]
  //-- nProc from [0..Quantity_Procedures-1]
  int _System ProcSnapshots (int nCom, int nModule, int nProc);

  //-- Procedure line bounds
  //-- nModule from [0..Quantity_Modules-1]
  //-- nProc from [0..Quantity_Procedures-1]
  //-- "begin" is line number of procedure BEGIN (counting from 0)
  //-- "end" is line number of procedure END
  //-- Line numbers are counted from 0
  BOOL _System ProcBounds (int nCom, int nModule, int nProc, PLONG begin, PLONG end);

} /* extern "C" */


#endif /* ifndef __PROFILE_H_ */

