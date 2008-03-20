/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: names.h,v 1.6 2008/03/20 02:57:38 kish_shen Exp $
 */

/*
 * IDENTIFICATION		names.h
 *
 * DESCRIPTION	
 *	
 *
 *
 * CONTENTS:
 *
 *
 * REVISION HISTORY:
 *
 * AUTHOR	VERSION	 DATE	REASON
 */

/*
 * INCLUDES:
 */

/*
 * DEFINES:
 */

/*
 * TYPEDEFS:
 */

/*
 * EXTERNAL FUNCTION DECLARATIONS: 
 */

/*
 * EXTERNAL VARIABLE DECLARATIONS: 
 */

/*
 * This array contains the mnemonics of all abstract instructions.
 * It has to be maintained together with opcode.h - if some opcodes
 * are changed the array has to be changed as well.
 *	SCCSid = "%W%	%G%"
 */
 
char * inst_name[] = {
"Code_end                  ",
"MoveAM                    ",
"MoveAMAM                  ",
"MoveAML                   ",
"MoveLAM                   ",
"MoveTMAM                  ",
"Get_variableNAML          ",
"Get_valueAMAM             ",
"Get_valueAML              ",
"Get_valueAMTM             ",
"Get_nilAM                 ",
"Get_integerAM             ",
"Get_floatAM               ",
"Get_atomAM                ",
"Get_stringAM              ",
"Get_listAM                ",
"Get_structureAM           ",
"In_get_nilAM              ",
"In_get_integerAM          ",
"In_get_floatAM            ",
"In_get_atomAM             ",
"In_get_stringAM           ",
"In_get_listAM             ",
"In_get_structureAM        ",
"Out_get_nilAM             ",
"Out_get_integerAM         ",
"Out_get_floatAM           ",
"Out_get_atomAM            ",
"Out_get_stringAM          ",
"Out_get_listAM            ",
"Out_get_structureAM       ",
"Get_list_argumentsAM      ",
"Get_structure_argumentsAM ",
"Write_void                ",
"Read_void                 ",
"Write_variable            ",
"Read_variable             ",
"Write_variableAM          ",
"Read_variableAM           ",
"Write_variableNL          ",
"Read_variableNL           ",
"Write_variableL           ",
"Read_variableL            ",
"Write_valueAM             ",
"Read_valueAM              ",
"Read_matched_valueAM      ",
"Write_local_valueAM       ",
"Write_valueL              ",
"Read_valueL               ",
"Read_matched_valueL       ",
"Write_local_valueL        ",
"Write_valueTM             ",
"Read_valueTM              ",
"Read_matched_valueTM      ",
"Write_local_valueTM       ",
"Write_nil                 ",
"Read_nil                  ",
"Write_integer             ",
"Read_integer              ",
"Write_float               ",
"Read_float                ",
"Write_did                 ",
"Read_atom                 ",
"Write_string              ",
"Read_string               ",
"Write_list                ",
"Write_structure           ",
"Read_list                 ",
"Read_listTM               ",
"Read_next_listTM          ",
"Read_last_list            ",
"Read_structure            ",
"Read_structureTM          ",
"Read_next_structureTM     ",
"Read_last_structure       ",
"Push_void                 ",
"Push_variableAM           ",
"Push_variableL            ",
"Push_variable             ",
"Push_valueAM              ",
"Push_valueL               ",
"Push_valueTM              ",
"Push_local_valueAM        ",
"Push_local_valueL         ",
"Push_local_valueTM        ",
"Push_nil                  ",
"Push_integer              ",
"Push_float                ",
"Push_init_variableL       ",
"Push_string               ",
"Push_list                 ",
"Push_structure            ",
"Bounce                    ",
"First                     ",
"NextTM                    ",
"ModeTM                    ",
"NextTMlab                 ",
"ModeTMlab                 ",
"Put_variableAML           ",
"Put_variableAM            ",
"Put_unsafe_valueAML       ",
"Put_nilAM                 ",
"Put_integerAM             ",
"Put_floatAM               ",
"Put_atomAM                ",
"Put_stringAM              ",
"Put_listAM                ",
"Put_structureAM           ",
"Puts_variable             ",
"Puts_variableL            ",
"Puts_valueAM              ",
"Puts_valueL               ",
"Puts_valueTM              ",
"Puts_nil                  ",
"Puts_integer              ",
"Puts_float                ",
"Puts_atom                 ",
"Puts_string               ",
"Puts_list                 ",
"Puts_structure            ",
"Integer_switchAM          ",
"Atom_switchAM             ",
"List_switchAM             ",
"Functor_switchAM          ",
"Switch_on_typeAM          ",
"Atom_switchL              ",
"Functor_switchL           ",
"Integer_switchL           ",
"Try_me_else               ",
"Try                       ",
"Trylab                    ",
"Retry_me_else             ",
"Retry                     ",
"Retrylab                  ",
"Trust_me                  ",
"Trust                     ",
"Allocate                  ",
"Space                     ",
"Initialize                ",
"Branch                    ",
"CallA                     ",
"CallP                     ",
"CallfA                    ",
"CallfP                    ",
"ChainA                    ",
"ChainP                    ",
"ChaincA                   ",
"ChaincP                   ",
"ChaindA                   ",
"ChaindP                   ",
"JmpA                      ",
"JmpP                      ",
"JmpdA                     ",
"JmpdP                     ",
"Exit                      ",
"Exitd                     ",
"Exitc                     ",
"Ret                       ",
"Retd                      ",
"Retn                      ",
"Savecut                   ",
"Neckcut                   ",
"Cut                       ",
"Failure                   ",
"Continue_after_event      ",
"Continue_after_event_debug",
"Escape                    ",
"List_switchL              ",
"External                  ",
"Puts_proc                 ",
"Debug_esc                 ",
"Gc                        ",
"Debug_call                ",
"Refail                    ",
"Exit_emulator             ",
"Debug_exit                ",
"Get_matched_valueAML      ",
"Nop                       ",
"Ress                      ",
"Deallocate                ",
"Get_constantAM            ",
"In_get_constantAM         ",
"Out_get_constantAM        ",
"Read_constant             ",
"Write_constant            ",
"Push_constant             ",
"Put_constantAM            ",
"Puts_constant             ",
"Get_matched_valueAMAM     ",
"Get_matched_valueAMTM     ",
"Puts_variableAM           ",
"Put_unsafe_valueAMTM      ",
"Branchs                   ",
"Gc_test                   ",
"Gc_testA                  ",
"Try_me_dynamic            ",
"Retry_me_dynamic          ",
"Read_test_var             ",
"Retry_me_inline           ",
"Trust_me_inline           ",
"Set_bp                    ",
"Restore_bp                ",
"New_bp                    ",
"SavecutL                  ",
"CutL                      ",
"JmpdAs                    ",
"Switch_on_typeL           ",
"Metacall                  ",
"Fastcall                  ",
"Integer_range_switchL     ",
"Suspension_call           ",
"Throw                     ",
"SavecutAM                 ",
"Cut_single                ",
"Initialize_named          ",
"Write_named_void          ",
"Write_named_variable      ",
"Write_named_variableAM    ",
"Write_named_variableL     ",
"Write_named_variableNL    ",
"Put_referenceAM           ",
"Put_referenceAML          ",
"Push_self_reference       ",
"Push_void_reference       ",
"Push_reference            ",
"Push_referenceAM          ",
"Push_referenceL           ",
"Puts_reference            ",
"Puts_referenceL           ",
"Occur_check_next          ",
"SoftcutL                  ",
"Dfid_testL                ",
"Dfid_test                 ",
"Depth                     ",
"Meta_jmpA                 ",
"Undefined                 ",
"Label                     ",		/* PSEUDO */
"Comment                   ",		/* PSEUDO */
"Reserve                   ",		/* PSEUDO */
"Get_metaAM                ",
"In_get_metaAM             ",
"Write_meta                ",
"Match_meta                ",
"Match_next_metaTM         ",
"Match_metaTM              ",
"Match_last_meta           ",
"Read_meta                 ",
"Read_next_metaTM          ",
"Read_metaTM               ",
"Read_last_meta            ",
"Continue_after_exception  ",
"CutAM                     ",
"Catch                     ",
"Res                       ",
"Handler_call              ",
"Retd_nowake               ",
"Push_init_referenceL      ",
"Exitd_nowake              ",
"Meta_jmp                  ",
"Suspension_jmp            ",
"Explicit_jmp              ",
"Read_referenceNL          ",
"Read_referenceL           ",
"Read_referenceAM          ",
"Read_reference            ",
"Read_voidN                ",
"Integer_range_switchAM    ",
"Puts_valueG               ",
"Push_valueG               ",
"GuardL                    ",
"Try_parallel              ",
"Retry_seq                 ",
"Fail_clause               ",
"Try_clause                ",
"Read_attribute            ",
"Wake_init                 ",
"Wake                      ",
"Ret_nowake                ",
"Neckcut_par               ",
"ExtCall                   ",
"External0                 ",
"External1                 ",
"External2                 ",
"External3                 ",
"Clause                    ",
"Put_global_variableAML    ",
"Put_global_variableL      ",
"Put_global_variableAM     ",
"MoveLL                    ",
"Get_valueLL               ",
"Escapef                   ",
"BI_Exit                   ",
"BI_PutCutAM               ",
"BI_PutCutL                ",
"BI_CutToStamp             ",
"BI_SetBipError            ",
"BI_GetBipError            ",
"BI_Free                   ",
"BI_Var                    ",
"BI_NonVar                 ",
"BI_Atom                   ",
"BI_Integer                ",
"BI_Float                  ",
"BI_Breal                  ",
"BI_Real                   ",
"BI_Rational               ",
"BI_String                 ",
"BI_Number                 ",
"BI_Atomic                 ",
"BI_Compound               ",
"BI_Meta                   ",
"BI_IsSuspension           ",
"BI_IsHandle               ",
"BI_IsEvent                ",
"BI_IsList                 ",
"BI_Identical              ",
"BI_NotIdentical           ",
"BI_Inequality             ",
"BI_NotIdentList           ",
"BI_ContDebug              ",
"BI_Minus                  ",
"BI_Succ                   ",
"BI_Add                    ",
"BI_Sub                    ",
"BI_Mul                    ",
"BI_Quot                   ",
"BI_Div                    ",
"BI_Rem                    ",
"BI_FloorDiv               ",
"BI_FloorRem               ",
"BI_And                    ",
"BI_Or                     ",
"BI_Xor                    ",
"BI_Bitnot                 ",
"BI_Lt                     ",
"BI_Le                     ",
"BI_Gt                     ",
"BI_Ge                     ",
"BI_Eq                     ",
"BI_Ne                     ",
"BI_Arg                    ",
"BI_MakeSuspension         ",
"Debug_scall               ",
"Retry_inline              ",
"Trust_inline              ",
"Put_named_variableAM      ",
"Put_named_variableL       ",
"Put_named_variableAML     ",
"Call_dynamic              ",
"Write_voidN               ",
"Push_voidN                ",
"MoveNLAM                  ",
"MoveNAML                  ",
"Move2LAM                  ",
"Move3LAM                  ",
"Move2AML                  ",
"Move3AML                  ",
"Move2AMAM                 ",
"Move3AMAM                 ",
"Move2LL                   ",
"Move3LL                   ",
"SwapAMAM                  ",
"ShiftAMAMAM               ",
"ShiftAMAMAMAM             ",
"ShiftAMAMAMAMAM           ",
"Read_variable2AML         ",
"Read_variable2AM          ",
"Read_variable2L           ",
"Write_variable2AML        ",
"Write_variable2AM         ",
"Write_variable2L          ",
"Write_local_value2AM      ",
"Write_local_value2L       ",
"Push_local_value2AM       ",
"Push_local_value2L        ",
"Put_global_variable2AML   ",
"Put_variable2AM           ",
"Get_integer2AM	           ",
"Get_atom2AM               ",
"Get_atomintegerAMAM       ",
"Get_integeratomAMAM       ",
"Write_first_structure     ",
"Write_first_list          ",
"Write_next_structureTM    ",
"Write_next_listTM         ",
"Write_next_structureTMlab ",
"Write_next_listTMlab      ",
"Read_atom2                ",
"Read_integer2             ",
"Read_integeratom          ",
"Read_atominteger          ",
"Write_did2                ",
"Write_integer2            ",
"Write_integerdid          ",
"Write_didinteger          ",
"MoveLAMCallfA             ",
"MoveLAMCallfP             ",
"MoveLAMChainA             ",
"MoveLAMChainP             ",
"MoveLAMJmpA               ",
"MoveLAMJmpP               ",
"Put_global_variableAMLCallfA",
"Put_global_variableAMLCallfP",
"Put_global_variableAMLChainA",
"Put_global_variableAMLChainP",
"Put_global_variableAMLJmpA ",
"Put_global_variableAMLJmpA ",   
"RotAMAMAM                 ",

#if (NREGARG > 0)
"MoveAR                    ",
"MoveARAM                  ",
"MoveARL                   ",
"MoveAMAR                  ",
"MoveLAR                   ",
"MoveTMAR                  ",
"Get_variableNARL          ",
"Get_valueAMAR             ",
"Get_valueARL              ",
"Get_valueARTM             ",
"Get_nilAR                 ",
"Get_integerAR             ",
"Get_floatAR               ",
"Get_atomAR                ",
"Get_stringAR              ",
"Get_listAR                ",
"Get_structureAR           ",
"In_get_nilAR              ",
"In_get_integerAR          ",
"In_get_floatAR            ",
"In_get_atomAR             ",
"In_get_stringAR           ",
"In_get_listAR             ",
"In_get_structureAR        ",
"Out_get_nilAR             ",
"Out_get_integerAR         ",
"Out_get_floatAR           ",
"Out_get_atomAR            ",
"Out_get_stringAR          ",
"Out_get_listAR            ",
"Out_get_structureAR       ",
"Get_list_argumentsAR      ",
"Get_structure_argumentsAR ",
"Write_variableAR          ",
"Read_variableAR           ",
"Write_valueAR             ",
"Read_valueAR              ",
"Read_matched_valueAR      ",
"Write_local_valueAR       ",
"Push_variableAR           ",
"Push_valueAR              ",
"Push_local_valueAR        ",
"Put_variableARL           ",
"Put_variableAR            ",
"Put_unsafe_valueARL       ",
"Put_nilAR                 ",
"Put_integerAR             ",
"Put_floatAR               ",
"Put_atomAR                ",
"Put_stringAR              ",
"Put_listAR                ",
"Put_structureAR           ",
"Puts_valueAR              ",
"Integer_switchAR          ",
"Atom_switchAR             ",
"List_switchAR             ",
"Functor_switchAR          ",
"Switch_on_typeAR          ",
"Atom_switch_seqAR         ",
"Functor_switch_seqAR      ",
"Integer_switch_seqAR      ",
"Get_matched_valueARL      ",
"Test_varAR                ",
"Test_groundAR             ",
"Get_matched_valueAMAR     ",
"Get_matched_valueARTM     ",
"Get_constantAR            ",
"In_get_constantAR         ",
"Out_get_constantAR        ",
"Put_constantAR            ",
"Puts_variableAR           ",
"Put_unsafe_valueARTM      ",
"Write_named_variableAR    ",
"Put_named_variableAR      ",
"Put_named_variableARL     ",
"Push_referenceAR          ",
#endif

#if (NREGTMP > 0)
"Write_variableTR          ",
"Read_variableTR           ",
"Write_valueTR             ",
"Read_valueTR              ",
"Read_matched_valueTR      ",
"Write_local_valueTR       ",
"Read_listTR               ",
"Read_next_listTR          ",
"Read_structureTR          ",
"Read_next_structureTR     ",
"Push_variableTR           ",
"Push_valueTR              ",
"Push_local_valueTR        ",
"Puts_valueTR              ",
"FirstTR                   ",
"NextTR                    ",
"ModeTR                    ",
"NextTRlab                 ",
"ModeTRlab                 ",
"Get_matched_valueAMTR     ",
"MoveAMTR                  ",
"MoveTRAM                  ",
"Get_valueAMTR             ",
"Write_named_variableTR    ",
"Push_referenceTR          ",
#endif


#if (NREGARG > 0) && (NREGTMP > 0)
"MoveARTR                  ",
"MoveTRAR                  ",
"Get_valueARTR             ",
"Get_matched_valueARTR     ",
#endif

#ifdef CHIP
#include "Cnames.h"
#endif /* CHIP */

#ifdef OBJECTS
/* additional machine instruction names for the objects extension */
#include "names_ob.h"
#endif /* OBJECTS */

"Inst_Error                "
};

