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
 * VERSION	$Id: opcode.h,v 1.5 2008/01/15 14:41:41 kish_shen Exp $
 */

/*
 * IDENTIFICATION		opcode.h
 *
 * DESCRIPTION	
 *	
 *	Here are defined the opcode values for the C emulator.
 *	They are defined in several blocks, the entries in each block
 *	are defined relative to the block beginning so that it is easy to
 *	insert new items inside a block. The blocks are:
 *	 - instructions without hardware registers, they are always defined
 *	   they range from 0 to OPCODES_WITHOUT_REGS
 *	 - instructions with AR registers (and no TR), they range
 *	   from BLOCK2 up to OPCODES_AR
 *	 - instructions with TR registers (and no AR), range
 *	   from BLOCK3 up to OPCODES_TR
 *	 - instructions with both AR and TR registers, range from BLOCK4
 *
 *	Note that the order of Write_, Read_ and Read_matched_ is significant.
 *
 * CONTENTS:
 *
 *
 * REVISION HISTORY:
 *
 * AUTHOR	VERSION	 DATE	REASON
 */

#ifdef THREADED

extern vmcode op_addr[];
#define Op_Value(x)	op_addr[x]
#define Get_Int_Opcode(code)	((vmcode) get_int_opcode(code))

extern vmcode bi_addr[];
#define Es_Value(x)	bi_addr[((int) (x))]

#else /* THREADED */

#define Op_Value(x)	(x)
#define Es_Value(x)	(x)
#define Get_Int_Opcode(code)	*(code)

#endif /* THREADED */

#define SameCode(x,i)		((x) == Op_Value(i))


/*
 * To have consecutive blocks of real instructions, we must set the
 * block beginnings properly.
 */
#define BLOCK1					0
#define BLOCK2					OPCODES_WITHOUT_REGS
#define BLOCK4					OPCODES_TR

#if (NREGARG == 0)
#if (NREGTMP == 0)
#define BLOCK3					OPCODES_AR
#define Inst_Error				OPCODES_WITHOUT_REGS
#else /* TR */
#define BLOCK3					OPCODES_WITHOUT_REGS
#define Inst_Error				OPCODES_TR
#endif
#else /* AR */
#if (NREGTMP == 0)
#define BLOCK3					OPCODES_WITHOUT_REGS
#define Inst_Error				OPCODES_WITHOUT_REGS
#else /* AR, TR */
#define BLOCK3					OPCODES_AR
#define Inst_Error				OPCODES_ARTR
#endif
#endif


/*
 * Inst_Error must be the last instruction of this file.
 * NUMBER_OP is the number of instructions, eventually set by an extension.
 */
#define Code_end				(BLOCK1 + 0)/*PSEUDO,must be 0*/
#define MoveAM					(BLOCK1 + 1)
#define MoveAMAM				(BLOCK1 + 2)
#define MoveAML					(BLOCK1 + 3)
#define MoveLAM					(BLOCK1 + 4)
#define MoveTMAM				(BLOCK1 + 5)
#define Get_variableNAML			(BLOCK1 + 6)
#define Get_valueAMAM				(BLOCK1 + 7)
#define Get_valueAML				(BLOCK1 + 8)
#define Get_valueAMTM				(BLOCK1 + 9)
#define Get_nilAM				(BLOCK1 + 10)
#define Get_integerAM				(BLOCK1 + 11)
#define Get_floatAM				(BLOCK1 + 12)
#define Get_atomAM				(BLOCK1 + 13)
#define Get_stringAM				(BLOCK1 + 14)
#define Get_listAM				(BLOCK1 + 15)
#define Get_structureAM				(BLOCK1 + 16)
#define In_get_nilAM				(BLOCK1 + 17)
#define In_get_integerAM			(BLOCK1 + 18)
#define In_get_floatAM				(BLOCK1 + 19)
#define In_get_atomAM				(BLOCK1 + 20)
#define In_get_stringAM				(BLOCK1 + 21)
#define In_get_listAM				(BLOCK1 + 22)
#define In_get_structureAM			(BLOCK1 + 23)
#define Out_get_nilAM				(BLOCK1 + 24)
#define Out_get_integerAM			(BLOCK1 + 25)
#define Out_get_floatAM				(BLOCK1 + 26)
#define Out_get_atomAM				(BLOCK1 + 27)
#define Out_get_stringAM			(BLOCK1 + 28)
#define Out_get_listAM				(BLOCK1 + 29)
#define Out_get_structureAM			(BLOCK1 + 30)
#define Get_list_argumentsAM			(BLOCK1 + 31)
#define Get_structure_argumentsAM		(BLOCK1 + 32)
/* The Write and Read instructions must be in the following order:
 *	Write_*
 *	Read_*
 *	Read_matched_*
 *	Write_local_*
 */
#define Write_void				(BLOCK1 + 33)
#define Read_void				(BLOCK1 + 34)
#define Write_variable				(BLOCK1 + 35)
#define Read_variable				(BLOCK1 + 36)
#define Write_variableAM			(BLOCK1 + 37)
#define Read_variableAM				(BLOCK1 + 38)
#define Write_variableNL			(BLOCK1 + 39)
#define Read_variableNL				(BLOCK1 + 40)
#define Write_variableL				(BLOCK1 + 41)
#define Read_variableL				(BLOCK1 + 42)
#define Write_valueAM				(BLOCK1 + 43)
#define Read_valueAM				(BLOCK1 + 44)
#define Read_matched_valueAM                    (BLOCK1 + 45)
#define Write_local_valueAM			(BLOCK1 + 46)
#define Write_valueL				(BLOCK1 + 47)
#define Read_valueL				(BLOCK1 + 48)
#define Read_matched_valueL                     (BLOCK1 + 49)
#define Write_local_valueL			(BLOCK1 + 50)
#define Write_valueTM				(BLOCK1 + 51)
#define Read_valueTM				(BLOCK1 + 52)
#define Read_matched_valueTM                    (BLOCK1 + 53)
#define Write_local_valueTM			(BLOCK1 + 54)
#define Write_nil				(BLOCK1 + 55)
#define Read_nil				(BLOCK1 + 56)
#define Write_integer				(BLOCK1 + 57)
#define Read_integer				(BLOCK1 + 58)
#define Write_float				(BLOCK1 + 59)
#define Read_float				(BLOCK1 + 60)
#define Write_did				(BLOCK1 + 61)
#define Read_atom				(BLOCK1 + 62)
#define Write_string				(BLOCK1 + 63)
#define Read_string				(BLOCK1 + 64)
#define Write_list				(BLOCK1 + 65)
#define Write_structure				(BLOCK1 + 66)
#define Read_list				(BLOCK1 + 67)
#define Read_listTM				(BLOCK1 + 68)
#define Read_next_listTM			(BLOCK1 + 69)
#define Read_last_list				(BLOCK1 + 70)
#define Read_structure				(BLOCK1 + 71)
#define Read_structureTM			(BLOCK1 + 72)
#define Read_next_structureTM			(BLOCK1 + 73)
#define Read_last_structure			(BLOCK1 + 74)
#define Push_void				(BLOCK1 + 75)
#define Push_variableAM				(BLOCK1 + 76)
#define Push_variableL				(BLOCK1 + 77)
#define Push_variable				(BLOCK1 + 78)
#define Push_valueAM				(BLOCK1 + 79)
#define Push_valueL				(BLOCK1 + 80)
#define Push_valueTM				(BLOCK1 + 81)
#define Push_local_valueAM			(BLOCK1 + 82)
#define Push_local_valueL			(BLOCK1 + 83)
#define Push_local_valueTM			(BLOCK1 + 84)
#define Push_nil				(BLOCK1 + 85)
#define Push_integer				(BLOCK1 + 86)
#define Push_float				(BLOCK1 + 87)
#define Push_init_variableL			(BLOCK1 + 88)
#define Push_string				(BLOCK1 + 89)
#define Push_list				(BLOCK1 + 90)
#define Push_structure				(BLOCK1 + 91)
#define Bounce					(BLOCK1 + 92)
#define First					(BLOCK1 + 93)
#define NextTM					(BLOCK1 + 94)
#define ModeTM					(BLOCK1 + 95)
#define NextTMlab				(BLOCK1 + 96)
#define ModeTMlab				(BLOCK1 + 97)
#define Put_variableAML				(BLOCK1 + 98)
#define Put_variableAM				(BLOCK1 + 99)
#define Put_unsafe_valueAML			(BLOCK1 + 100)
#define Put_nilAM				(BLOCK1 + 101)
#define Put_integerAM				(BLOCK1 + 102)
#define Put_floatAM				(BLOCK1 + 103)
#define Put_atomAM				(BLOCK1 + 104)
#define Put_stringAM				(BLOCK1 + 105)
#define Put_listAM				(BLOCK1 + 106)
#define Put_structureAM				(BLOCK1 + 107)
#define Puts_variable				(BLOCK1 + 108)
#define Puts_variableL				(BLOCK1 + 109)
#define Puts_valueAM				(BLOCK1 + 110)
#define Puts_valueL				(BLOCK1 + 111)
#define Puts_valueTM				(BLOCK1 + 112)
#define Puts_nil				(BLOCK1 + 113)
#define Puts_integer				(BLOCK1 + 114)
#define Puts_float				(BLOCK1 + 115)
#define Puts_atom				(BLOCK1 + 116)
#define Puts_string				(BLOCK1 + 117)
#define Puts_list				(BLOCK1 + 118)
#define Puts_structure				(BLOCK1 + 119)
#define Integer_switchAM			(BLOCK1 + 120)
#define Atom_switchAM				(BLOCK1 + 121)
#define List_switchAM				(BLOCK1 + 122)
#define Functor_switchAM			(BLOCK1 + 123)
#define Switch_on_typeAM			(BLOCK1 + 124)
#define Atom_switchL				(BLOCK1 + 125)
#define Functor_switchL				(BLOCK1 + 126)
#define Integer_switchL				(BLOCK1 + 127)
#define Try_me_else				(BLOCK1 + 128)
#define Try					(BLOCK1 + 129)
#define Trylab					(BLOCK1 + 130)
#define Retry_me_else				(BLOCK1 + 131)
#define Retry					(BLOCK1 + 132)
#define Retrylab				(BLOCK1 + 133)
#define Trust_me				(BLOCK1 + 134)
#define Trust					(BLOCK1 + 135)
#define Allocate				(BLOCK1 + 136)
#define Space					(BLOCK1 + 137)
#define Initialize				(BLOCK1 + 138)
#define Branch					(BLOCK1 + 139)
#define CallA					(BLOCK1 + 140) /* Call etc.  */
#define CallP					(BLOCK1 + 141) /* must stay  */
#define CallfA					(BLOCK1 + 142) /* together   */
#define CallfP					(BLOCK1 + 143) /* (peephole) */
#define ChainA					(BLOCK1 + 144)
#define ChainP					(BLOCK1 + 145)
#define ChaincA					(BLOCK1 + 146)
#define ChaincP					(BLOCK1 + 147)
#define ChaindA					(BLOCK1 + 148)
#define ChaindP					(BLOCK1 + 149)
#define JmpA					(BLOCK1 + 150)
#define JmpP					(BLOCK1 + 151)
#define JmpdA					(BLOCK1 + 152)
#define JmpdP					(BLOCK1 + 153)
#define Exit					(BLOCK1 + 154)
#define Exitd					(BLOCK1 + 155)
#define Exitc					(BLOCK1 + 156)
#define Ret					(BLOCK1 + 157)
#define Retd					(BLOCK1 + 158)
#define Retn					(BLOCK1 + 159)
#define Savecut					(BLOCK1 + 160)
#define Neckcut					(BLOCK1 + 161)
#define Cut					(BLOCK1 + 162)
#define Failure					(BLOCK1 + 163)
#define Continue_after_event			(BLOCK1 + 164)
#define Continue_after_event_debug		(BLOCK1 + 165)
#define Escape					(BLOCK1 + 166)
#define List_switchL				(BLOCK1 + 167)
#define External				(BLOCK1 + 168)
#define Puts_proc				(BLOCK1 + 169)
#define Debug_esc				(BLOCK1 + 170)
#define Gc					(BLOCK1 + 171)
#define Debug_call				(BLOCK1 + 172)
#define Refail					(BLOCK1 + 173)
#define Exit_emulator				(BLOCK1 + 174)
#define Debug_exit				(BLOCK1 + 175)
#define Get_matched_valueAML                    (BLOCK1 + 176)
#define Nop					(BLOCK1 + 177)
#define Ress					(BLOCK1 + 178)
#define Deallocate				(BLOCK1 + 179)
#define Get_constantAM				(BLOCK1 + 180)
#define In_get_constantAM			(BLOCK1 + 181)
#define Out_get_constantAM			(BLOCK1 + 182)
#define Read_constant				(BLOCK1 + 183)
#define Write_constant				(BLOCK1 + 184)
#define Push_constant				(BLOCK1 + 185)
#define Put_constantAM				(BLOCK1 + 186)
#define Puts_constant				(BLOCK1 + 187)
#define Get_matched_valueAMAM                   (BLOCK1 + 188)
#define Get_matched_valueAMTM                   (BLOCK1 + 189)
#define Puts_variableAM                   	(BLOCK1 + 190)
#define Put_unsafe_valueAMTM                   	(BLOCK1 + 191)
#define Branchs		                   	(BLOCK1 + 192)
#define Gc_test					(BLOCK1 + 193)
#define Gc_testA				(BLOCK1 + 194)
#define Try_me_dynamic				(BLOCK1 + 195)
#define Retry_me_dynamic			(BLOCK1 + 196)
#define Read_test_var				(BLOCK1 + 197)
#define Retry_me_inline				(BLOCK1 + 198)
#define Trust_me_inline				(BLOCK1 + 199)
#define Set_bp					(BLOCK1 + 200)
#define Restore_bp				(BLOCK1 + 201)
#define New_bp					(BLOCK1 + 202)
#define SavecutL				(BLOCK1 + 203)
#define CutL					(BLOCK1 + 204)
#define JmpdAs					(BLOCK1 + 205)
#define Switch_on_typeL				(BLOCK1 + 206)
#define Metacall				(BLOCK1 + 207)
#define Fastcall				(BLOCK1 + 208)
#define Integer_range_switchL			(BLOCK1 + 209)
#define Suspension_call				(BLOCK1 + 210)
#define Throw					(BLOCK1 + 211)
#define SavecutAM				(BLOCK1 + 212)
#define Cut_single				(BLOCK1 + 213)
#define Initialize_named			(BLOCK1 + 214)
#define Write_named_void			(BLOCK1 + 215)
#define Write_named_variable			(BLOCK1 + 216)
#define Write_named_variableAM			(BLOCK1 + 217)
#define Write_named_variableL			(BLOCK1 + 218)
#define Write_named_variableNL			(BLOCK1 + 219)
#define Put_referenceAM				(BLOCK1 + 220)
#define Put_referenceAML			(BLOCK1 + 221)
#define Push_self_reference			(BLOCK1 + 222)
#define Push_void_reference			(BLOCK1 + 223)
#define Push_reference				(BLOCK1 + 224)
#define Push_referenceAM			(BLOCK1 + 225)
#define Push_referenceL				(BLOCK1 + 226)
#define Puts_reference				(BLOCK1 + 227)
#define Puts_referenceL				(BLOCK1 + 228)
#define Occur_check_next			(BLOCK1 + 229)
#define SoftcutL				(BLOCK1 + 230)
#define Dfid_testL				(BLOCK1 + 231)
#define Dfid_test				(BLOCK1 + 232)
#define Depth					(BLOCK1 + 233)
#define Meta_jmpA				(BLOCK1 + 234)
#define Undefined				(BLOCK1 + 235)
#define Label					(BLOCK1 + 236) /* PSEUDO */
#define Comment					(BLOCK1 + 237) /* PSEUDO */
#define Reserve					(BLOCK1 + 238) /* PSEUDO */
#define Get_metaAM				(BLOCK1 + 239)
#define In_get_metaAM				(BLOCK1 + 240)
#define Write_meta				(BLOCK1 + 241)
#define Match_meta				(BLOCK1 + 242)
#define Match_next_metaTM			(BLOCK1 + 243)
#define Match_metaTM				(BLOCK1 + 244)
#define Match_last_meta				(BLOCK1 + 245)
#define Read_meta				(BLOCK1 + 246)
#define Read_next_metaTM			(BLOCK1 + 247)
#define Read_metaTM				(BLOCK1 + 248)
#define Read_last_meta				(BLOCK1 + 249)
#define Continue_after_exception		(BLOCK1 + 250)
#define CutAM					(BLOCK1 + 251)
#define Catch					(BLOCK1 + 252)
#define Res					(BLOCK1 + 253)
#define Handler_call				(BLOCK1 + 254)
#define Retd_nowake				(BLOCK1 + 255)
#define Push_init_referenceL			(BLOCK1 + 256)
#define Exitd_nowake				(BLOCK1 + 257)
#define Meta_jmp				(BLOCK1 + 258)
#define Suspension_jmp				(BLOCK1 + 259)
#define Explicit_jmp				(BLOCK1 + 260)
#define Read_referenceNL			(BLOCK1 + 261)
#define Read_referenceL				(BLOCK1 + 262)
#define Read_referenceAM			(BLOCK1 + 263)
#define Read_reference				(BLOCK1 + 264)
#define Read_voidN				(BLOCK1 + 265)
#define Integer_range_switchAM			(BLOCK1 + 266)
#define Puts_valueG				(BLOCK1 + 267)
#define Push_valueG				(BLOCK1 + 268)
#define GuardL					(BLOCK1 + 269)
#define Try_parallel				(BLOCK1 + 270)
#define Retry_seq				(BLOCK1 + 271)
#define Fail_clause				(BLOCK1 + 272)
#define Try_clause				(BLOCK1 + 273)
#define Read_attribute				(BLOCK1 + 274)
#define Wake_init				(BLOCK1 + 275)
#define Wake					(BLOCK1 + 276)
#define Ret_nowake				(BLOCK1 + 277)
#define Neckcut_par				(BLOCK1 + 278)
#define ExtCall					(BLOCK1 + 279)
#define External0				(BLOCK1 + 280)
#define External1				(BLOCK1 + 281)
#define External2				(BLOCK1 + 282)
#define External3				(BLOCK1 + 283)
#define Clause					(BLOCK1 + 284)
#define Put_global_variableAML			(BLOCK1 + 285)
#define Put_global_variableL			(BLOCK1 + 286)
#define Put_global_variableAM			(BLOCK1 + 287)
#define MoveLL					(BLOCK1 + 288)
#define Get_valueLL				(BLOCK1 + 289)
#define Escapef					(BLOCK1 + 290)
#define BI_Exit					(BLOCK1 + 291)
#define BI_PutCutAM				(BLOCK1 + 292)
#define BI_PutCutL				(BLOCK1 + 293)
#define BI_CutToStamp				(BLOCK1 + 294)
#define BI_SetBipError				(BLOCK1 + 295)
#define BI_GetBipError				(BLOCK1 + 296)
#define BI_Free					(BLOCK1 + 297)
#define BI_Var					(BLOCK1 + 298)
#define BI_NonVar				(BLOCK1 + 299)
#define BI_Atom					(BLOCK1 + 300)
#define BI_Integer				(BLOCK1 + 301)
#define BI_Float				(BLOCK1 + 302)
#define BI_Breal				(BLOCK1 + 303)
#define BI_Real					(BLOCK1 + 304)
#define BI_Rational				(BLOCK1 + 305)
#define BI_String				(BLOCK1 + 306)
#define BI_Number				(BLOCK1 + 307)
#define BI_Atomic				(BLOCK1 + 308)
#define BI_Compound				(BLOCK1 + 309)
#define BI_Meta					(BLOCK1 + 310)
#define BI_IsSuspension				(BLOCK1 + 311)
#define BI_IsHandle				(BLOCK1 + 312)
#define BI_IsEvent				(BLOCK1 + 313)
#define BI_IsList				(BLOCK1 + 314)
#define BI_Identical				(BLOCK1 + 315)
#define BI_NotIdentical				(BLOCK1 + 316)
#define BI_Inequality				(BLOCK1 + 317)
#define BI_NotIdentList				(BLOCK1 + 318)
#define BI_ContDebug				(BLOCK1 + 319)
#define BI_Minus				(BLOCK1 + 320)
#define BI_Succ					(BLOCK1 + 321)
#define BI_Add					(BLOCK1 + 322)
#define BI_Sub					(BLOCK1 + 323)
#define BI_Mul					(BLOCK1 + 324)
#define BI_Quot					(BLOCK1 + 325)
#define BI_Div					(BLOCK1 + 326)
#define BI_Rem					(BLOCK1 + 327)
#define BI_FloorDiv				(BLOCK1 + 328)
#define BI_FloorRem				(BLOCK1 + 329)
#define BI_And					(BLOCK1 + 330)
#define BI_Or					(BLOCK1 + 331)
#define BI_Xor					(BLOCK1 + 332)
#define BI_Bitnot				(BLOCK1 + 333)
#define BI_Lt					(BLOCK1 + 334)
#define BI_Le					(BLOCK1 + 335)
#define BI_Gt					(BLOCK1 + 336)
#define BI_Ge					(BLOCK1 + 337)
#define BI_Eq					(BLOCK1 + 338)
#define BI_Ne					(BLOCK1 + 339)
#define BI_Arg					(BLOCK1 + 340)
#define BI_MakeSuspension			(BLOCK1 + 341)
#define Debug_scall				(BLOCK1 + 342)
#define Retry_inline				(BLOCK1 + 343)
#define Trust_inline				(BLOCK1 + 344)
#define Put_named_variableAM			(BLOCK1 + 345)
#define Put_named_variableL			(BLOCK1 + 346)
#define Put_named_variableAML			(BLOCK1 + 347)
#define Call_dynamic				(BLOCK1 + 348)
#define Write_voidN		       		(BLOCK1 + 349)
#define Push_voidN		       		(BLOCK1 + 350)
#define MoveNLAM				(BLOCK1 + 351)
#define MoveNAML				(BLOCK1 + 352)
#define Move2LAM				(BLOCK1 + 353)
#define Move3LAM				(BLOCK1 + 354)
#define Move2AML				(BLOCK1 + 355)
#define Move3AML				(BLOCK1 + 356)
#define OPCODES_WITHOUT_REGS			(BLOCK1 + 357)

/*
 * Don't forget to update the following files:
 * names.h emu_op_addr.h printam.c pass4.c asm.pl
 */

#define Escapes					Escapeu

#define MoveAR					BLOCK2
#define MoveARAM				(BLOCK2 + 1)
#define MoveARL					(BLOCK2 + 2)
#define MoveAMAR				(BLOCK2 + 3)
#define MoveLAR					(BLOCK2 + 4)
#define MoveTMAR				(BLOCK2 + 5)
#define Get_variableNARL			(BLOCK2 + 6)
#define Get_valueAMAR				(BLOCK2 + 7)
#define Get_valueARL				(BLOCK2 + 8)
#define Get_valueARTM				(BLOCK2 + 9)
#define Get_nilAR				(BLOCK2 + 10)
#define Get_integerAR				(BLOCK2 + 11)
#define Get_floatAR				(BLOCK2 + 12)
#define Get_atomAR				(BLOCK2 + 13)
#define Get_stringAR				(BLOCK2 + 14)
#define Get_listAR				(BLOCK2 + 15)
#define Get_structureAR				(BLOCK2 + 16)
#define In_get_nilAR				(BLOCK2 + 17)
#define In_get_integerAR			(BLOCK2 + 18)
#define In_get_floatAR				(BLOCK2 + 19)
#define In_get_atomAR				(BLOCK2 + 20)
#define In_get_stringAR				(BLOCK2 + 21)
#define In_get_listAR				(BLOCK2 + 22)
#define In_get_structureAR			(BLOCK2 + 23)
#define Out_get_nilAR				(BLOCK2 + 24)
#define Out_get_integerAR			(BLOCK2 + 25)
#define Out_get_floatAR				(BLOCK2 + 26)
#define Out_get_atomAR				(BLOCK2 + 27)
#define Out_get_stringAR			(BLOCK2 + 28)
#define Out_get_listAR				(BLOCK2 + 29)
#define Out_get_structureAR			(BLOCK2 + 30)
#define Get_list_argumentsAR			(BLOCK2 + 31)
#define Get_structure_argumentsAR		(BLOCK2 + 32)
#define Write_variableAR			(BLOCK2 + 33)
#define Read_variableAR				(BLOCK2 + 34)
#define Write_valueAR				(BLOCK2 + 35)
#define Read_valueAR				(BLOCK2 + 36)
#define Read_matched_valueAR			(BLOCK2 + 37)
#define Write_local_valueAR			(BLOCK2 + 38)
#define Push_variableAR				(BLOCK2 + 39)
#define Push_valueAR				(BLOCK2 + 40)
#define Push_local_valueAR			(BLOCK2 + 41)
#define Put_variableARL				(BLOCK2 + 42)
#define Put_variableAR				(BLOCK2 + 43)
#define Put_unsafe_valueARL			(BLOCK2 + 44)
#define Put_nilAR				(BLOCK2 + 45)
#define Put_integerAR				(BLOCK2 + 46)
#define Put_floatAR				(BLOCK2 + 47)
#define Put_atomAR				(BLOCK2 + 48)
#define Put_stringAR				(BLOCK2 + 49)
#define Put_listAR				(BLOCK2 + 50)
#define Put_structureAR				(BLOCK2 + 51)
#define Puts_valueAR				(BLOCK2 + 52)
#define Integer_switchAR			(BLOCK2 + 53)
#define Atom_switchAR				(BLOCK2 + 54)
#define List_switchAR				(BLOCK2 + 55)
#define Functor_switchAR			(BLOCK2 + 56)
#define Switch_on_typeAR			(BLOCK2 + 57)
#define Atom_switch_seqAR			(BLOCK2 + 58)
#define Functor_switch_seqAR			(BLOCK2 + 59)
#define Integer_switch_seqAR			(BLOCK2 + 60)
#define Get_matched_valueARL			(BLOCK2 + 61)
#define Test_varAR				(BLOCK2 + 62)
#define Test_groundAR				(BLOCK2 + 63)
#define Get_matched_valueAMAR                   (BLOCK2 + 64)
#define Get_matched_valueARTM                   (BLOCK2 + 65)
#define Get_constantAR				(BLOCK2 + 66)
#define In_get_constantAR			(BLOCK2 + 67)
#define Out_get_constantAR			(BLOCK2 + 68)
#define Put_constantAR				(BLOCK2 + 69)
#define Puts_variableAR				(BLOCK2 + 70)
#define Put_unsafe_valueARTM                   	(BLOCK2 + 71)
#define Write_named_variableAR			(BLOCK2 + 72)
#define Put_named_variableAR			(BLOCK2 + 73)
#define Put_named_variableARL			(BLOCK2 + 74)
#define Push_referenceAR			(BLOCK2 + 75)
#define OPCODES_AR				(BLOCK2 + 76)

#define Write_variableTR			BLOCK3
#define Read_variableTR				(BLOCK3 + 1)
#define Write_valueTR				(BLOCK3 + 2)
#define Read_valueTR				(BLOCK3 + 3)
#define Read_matched_valueTR			(BLOCK3 + 4)
#define Write_local_valueTR			(BLOCK3 + 5)
#define Read_listTR				(BLOCK3 + 6)
#define Read_next_listTR			(BLOCK3 + 7)
#define Read_structureTR			(BLOCK3 + 8)
#define Read_next_structureTR			(BLOCK3 + 9)
#define Push_variableTR				(BLOCK3 + 10)
#define Push_valueTR				(BLOCK3 + 11)
#define Push_local_valueTR			(BLOCK3 + 12)
#define Puts_valueTR				(BLOCK3 + 13)
#define FirstTR					(BLOCK3 + 14)
#define NextTR					(BLOCK3 + 15)
#define ModeTR					(BLOCK3 + 16)
#define NextTRlab				(BLOCK3 + 17)
#define ModeTRlab				(BLOCK3 + 18)
#define Get_matched_valueAMTR			(BLOCK3 + 19)
#define MoveAMTR				(BLOCK3 + 20)
#define MoveTRAM				(BLOCK3 + 21)
#define Get_valueAMTR				(BLOCK3 + 22)
#define Write_named_variableTR			(BLOCK3 + 23)
#define Push_referenceTR			(BLOCK3 + 24)
#define OPCODES_TR				(BLOCK3 + 25)

#define MoveARTR				BLOCK4
#define MoveTRAR				(BLOCK4 + 1)
#define Get_valueARTR				(BLOCK4 + 2)
#define Get_matched_valueARTR			(BLOCK4 + 3)
#define OPCODES_ARTR				(BLOCK4 + 4)

/*
 * Expanded built-in predicate opcodes
 */
#define BIExit			1
#define BIGetCut		2
#define BIFloorDiv		3
#define BIFloorRem		4
#define BIParentCreep		5	/* unused */
#define BIGetParentDebug	6	/* unused */
#define BISetBipError		7
#define BIGetBipError		8
#define BIFail			9
#define BIVar			10
#define BINonVar		11
#define BIAtom			12
#define BIInteger		13
#define BIReal			14
#define BIString		15
#define BINumber		16
#define BIAtomic		17
#define BICompound		18
#define BIIdentical		19
#define BINotIdentical		20
#define	BISucc			21
#define BIUnify			22
#define BISave			23
#define BIRestore		24
#define	BIMinus			25
#define	BIAdd			26
#define	BISub			27
#define	BIMul			28
#define	BIQuot			29
#define	BIDiv			30
#define	BIRem			31
#define	BIAnd			32
#define	BIOr			33
#define	BIXor			34
#define	BIRshift		35
#define	BILshift		36
#define	BIBitnot		37
#define	BIFloat			38
#define	BILt			39
#define	BILe			40
#define	BIGt			41
#define	BIGe			42
#define	BIEq			43
#define	BINe			44
#define	BIArg			45
#define	BIMeta			46
#define	BIInequality		47
#define	BIFree			48
#define	BIMakeSuspension	49
#define	BIRational		50
#define	BINotIdentList		51
#define	BIIsSuspension		52
#define	BIContDebug		53
#define	BICutToStamp		54
#define	BIIsEvent		55
#define	BIIsHandle		56
#define	BIBreal			57
#define	BIIsList		58
#define LAST_EXPANDED_OPCODE	58 /* used in printam.c */

#define NUMBER_OP				(Inst_Error + 1)
