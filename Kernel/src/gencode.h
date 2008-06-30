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
 * VERSION	$Id: gencode.h,v 1.1 2008/06/30 17:43:55 jschimpf Exp $
 */

/*
 * IDENTIFICATION		gencode.h
 *
 * DESCRIPTION	
 *
 * The definition of abstract instructions generated when Prolog
 * source is compiled.
 *
 * CONTENTS:
 *
 *
 * REVISION HISTORY:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * Micha Meier	1.0		created the file
 * Micha Meier	2.2	20.7.89	updated for the new compiler
 */

/*
 * DEFINES:
 */
#define Nreg			(NREGARG + NREGTMP)

#if Nreg
#define NREG_DFLT		(Nreg + 1)
#else
#define NREG_DFLT		2
#endif

#if (NREGTMP > 0)
#define HasTempReg		1	/* for multiplication */
#define FirstTemp		-1
#else
#define HasTempReg		0
#define FirstTemp		1
#endif

#if (NREGARG > 0)
#define HasArgReg		1 	/* for multiplication */
#else
#define HasArgReg		0
#endif

#define DEBUG_LENGTH		3

#define FailCode		fail_code_

#define CONT_R			0x80000000L
#define Esize(size)		((vmcode)((size) * (long)sizeof(pword)))
#define MemoryA(arg)		(arg > NREGARG)
#define MemoryT(temp)		(((temp) & CONT_R) == 0)
#define Address(arg)		(&g_emu_.emu_args[arg])
#define VarPermOff(tvv)		Esize(VarPermNo(tvv))
#define TempOff(tmp, p)		Esize((p) - NumberOf(tmp))
#define GTempOff(tmp, s)	Esize((tmp) - (s))
#define DirectLink(gval, procedure, proc)	\
		((gval) == procedure->did &&\
		!DynamicProc(proc) &&\
		procedure->module == proc->module_ref)

#define PCodeA(Gproc)		(PriCode(Gproc))
#define PCodeP(Gproc)		(Gproc)

/* This could be defined to yield true for predicates which have VMCODE
 * that will never change (never be recompiled etc). Currently we don't
 * guarantee that for any predicate.
 */
#define LoadedPermanent(gproc)	0

/* macro for generating vmcode data words */
#define Store_d(x)		*code++ = (vmcode) (long) (x);

/* macro for generating vmcode instruction words */
#ifdef THREADED
#define Store_i(x)		*code++ = Op_Value(x);
#else
#define Store_i(x)		Store_d(x)
#endif
#define Store_W(x)		wptr++;

 /* this one to avoid ambiguity in code++ */
#define Store_Lab(codeptr)	*code++ = (vmcode) (codeptr);

#define Store_2(i,x)		Store_i(i) Store_d(x)
#define Store_3(i,x,y)		Store_i(i) Store_d(x) Store_d(y)
#define Store_4(i,x,y,z)	Store_i(i) Store_d(x) Store_d(y) Store_d(z)
#define Store_2d(i,x)		Store_d(i) Store_d(x)
#define Store_3d(i,x,y)		Store_d(i) Store_d(x) Store_d(y)
#define Store_4d(i,x,y,z)       Store_d(i) Store_d(x) Store_d(y) Store_d(z)

#define Store_Last_Goal(IfA, IfP, Gproc, did, module, port, debug, gflags)\
		    Debug_Call(Gproc, port, debug, gflags)	\
		    if (DirectLink(gval, procedure, Gproc)) {	\
			    Store_d(IfA)			\
			    Set_Label((procedure)->start)	\
		    } else if (LoadedPermanent(Gproc))			\
			    {Store_2d(IfA, PCodeA(Gproc))}\
		    else				\
			    {Store_2d(IfP, PCodeP(Gproc))}
#if (NREGARG > 0)
#define Store_Arg_1(IfA, IfrA, arg)	\
				if (MemoryA(arg))\
					{Store_2(IfA, Address(arg))}\
				else\
					{Store_i(IfrA)}
#define Store_Arg_1d(IfA, IfrA, arg)	\
				if (MemoryA(arg))\
					{Store_2d(IfA, Address(arg))}\
				else\
					{Store_d(IfrA)}
#define Store_Arg_2(IfA, IfrA, arg, item)	\
				if (MemoryA(arg))\
					{Store_3(IfA, Address(arg), item)}\
				else\
					{Store_2(IfrA, item)}
#define Store_Arg_2d(IfA, IfrA, arg, item)	\
				if (MemoryA(arg))\
					{Store_3d(IfA, Address(arg), item)}\
				else\
					{Store_2d(IfrA, item)}
#define Store_Arg_3d(IfA, IfrA, arg, item1, item2)	\
                                if (MemoryA(arg))\
					{Store_4d(IfA, Address(arg), item1, item2)}\
				else\
					{Store_3d(IfrA, item1, item2)}
#define Store_Rev_Arg_2(IfA, IfrA, item, arg)	\
				if (MemoryA(arg))\
					{Store_3(IfA, item, Address(arg))}\
				else\
					{Store_2(IfrA, item)}
#define Store_Rev_Arg_3(IfA, IfrA, item1, arg, item2)	\
				if (MemoryA(arg))\
					{Store_4(IfA, item1, Address(arg), item2)}\
				else\
					{Store_3(IfrA, item1, item2)}
#define Store_Rev_Arg_2d(IfA, IfrA, item, arg)	\
				if (MemoryA(arg))\
					{Store_3d(IfA, item, Address(arg))}\
				else\
					{Store_2d(IfrA, item)}
#define Store_Rev_Arg_3d(IfA, IfrA, item1, arg, item2)	\
				if (MemoryA(arg))\
					{Store_4d(IfA, item1, Address(arg), item2)}\
				else\
					{Store_3d(IfrA, item1, item2)}
#else
#define Store_Arg_1(IfA, IfrA, arg)		Store_2(IfA, Address(arg))
#define Store_Arg_1d(IfA, IfrA, arg)		{Store_2d(IfA, Address(arg))}
#define Store_Arg_2(IfA, IfrA, arg, item)	Store_3(IfA, Address(arg), item)
#define Store_Arg_2d(IfA, IfrA, arg, item)		\
				{Store_3d(IfA, Address(arg), item)}
#define Store_Arg_3d(IfA, IfrA, arg, item1, item2)	\
				Store_4d(IfA, Address(arg), item1, item2)
#define Store_Rev_Arg_2(IfA, IfrA, item, arg)	Store_3(IfA, item, Address(arg))
#define Store_Rev_Arg_2d(IfA, IfrA, item, arg)	Store_3d(IfA, item, Address(arg))
#define Store_Rev_Arg_3(IfA, IfrA, item1, arg, item2)	\
				Store_4(IfA, item1, Address(arg), item2)
#define Store_Rev_Arg_3d(IfA, IfrA, item1, arg, item2)	\
				Store_4d(IfA, item1, Address(arg), item2)
#endif
#define Store_First_Temp_1d(IfT, IfrT, temp)	\
				if (MemoryT(temp))\
					{Store_d(IfT)}\
				else\
					{Store_d(IfrT)}
#define Store_First_Temp_2d(IfT, IfrT, temp, n)	\
				if (MemoryT(temp))\
					{Store_2d(IfT, n)}\
				else\
					{Store_2d(IfrT, n)}
#define Store_Temp_1d(IfT, IfrT, temp, p)	\
				if (MemoryT(temp))\
					{Store_2d(IfT, TempOff(temp, p))}\
				else\
					{Store_d(IfrT)}
#define Store_Temp_2d(IfT, IfrT, temp, p, item)	\
				if (MemoryT(temp))\
					{Store_3d(IfT, TempOff(temp, p), item)}\
				else\
					{Store_2d(IfrT, item)}
#define Store_Two_Args(IfAA, IfArA, IfrAA, IfrArA, arg1, arg2)		\
		if (MemoryA(arg1))\
		{\
		    if (MemoryA(arg2))\
			{Store_3d(IfAA, Address(arg1), Address(arg2))}\
		    else\
			{Store_2d(IfArA, Address(arg1))}\
		}\
		else if (MemoryA(arg2))\
		    {Store_2d(IfrAA, Address(arg2))}\
		else\
		    {Store_d(IfrArA)}
#define Store_Arg_Temp(IfAT, IfArT, IfrAT, IfrArT, arg, temp, p)	\
		if (MemoryA(arg))\
		{\
			if (MemoryT(temp))\
				{Store_3d(IfAT, Address(arg), TempOff(temp, p))}\
			else\
				{Store_2d(IfArT, Address(arg))}\
		}\
		else if (MemoryT(temp))\
			{Store_2d(IfrAT, TempOff(temp, p))}\
		else\
			{Store_d(IfrArT)}
#define Store_Temp_Arg(IfTA, IfrTA, IfTrA, IfrTrA, temp, p, arg)	\
		if (MemoryA(arg))\
		{\
			if (MemoryT(temp))\
				{Store_3d(IfTA, TempOff(temp, p),	\
					Address(arg))}\
			else\
				{Store_2d(IfrTA, Address(arg))}\
		}\
		else if (MemoryT(temp))\
			{Store_2d(IfTrA, TempOff(temp, p))}\
		else\
			{Store_d(IfrTrA)}
#define Store_First_Arg_Temp(IfAT, IfArT, IfrAT, IfrArT, arg, temp, p)	\
				if (MemoryA(arg))\
				{\
					if (MemoryT(temp))\
						{Store_2d(IfAT, Address(arg))}\
					else\
						{Store_2d(IfArT, Address(arg))}\
				}\
				else if (MemoryT(temp))\
					{Store_d(IfrAT)}\
				else\
					{Store_d(IfrArT)}


#define LABEL_SIZE		3
/* a label without any pending references */
#define New_Label(lab)		(lab) = code; Store_3d(Label, -1, 0); \
				procedure->size -= LABEL_SIZE;
/* a label with a pending reference */
#define Label_(addr)		*(addr) = (vmcode) code;\
				Store_3d(Label, 0, 0);\
				procedure->size -= LABEL_SIZE;
/* reference to a defined label, not at the current insertion point */
#define Add_Label(addr, lab)	*(addr) = (vmcode) (lab);\
				*((lab) + 1) = 0;
/* reference to a defined label */
#define Set_Label(lab)		Add_Label(code, lab); code++;
/* reference to an undefined label */
#define Forward_Label(addr)	*code = 0; (addr) = code++;
/* reference to a label of another instruction */
#define Copy_Label(to, addr)	*(to) = *(addr);

#define Reserve_(i)		Store_2d(Reserve, i); code += (i) - 2;
#define Comment_(addr)		{word off=addr-code-2; Store_2d(Comment, off)}
				

/* CONTROL INSTRUCTIONS */

#define Allocate_(size)		Store_2d(Allocate, Esize(size))
/* in the write sequence - always the index */
#define Space_(size)		\
	{\
	    if (size)\
		{Store_2d(Space, Esize(size))}\
	}
#define Initialize_(off, mask)	\
	if (GlobalFlags & VARIABLE_NAMES) \
	    {Store_3d(Initialize_named, off, mask)}\
	else \
	    {Store_3d(Initialize, off, mask)}
#define Gc_Test(clds, goaln)					\
    {								\
	if ((clds)->gc_list && (clds)->gc_list->goal == (goaln))\
	{							\
	    Store_2d(Gc_test, Esize((clds)->gc_list->size));	\
	    (clds)->gc_list = (clds)->gc_list->next;		\
	}							\
    }
#define Gc_TestA(clds, arity)					\
    {								\
	if ((clds)->gc_list && ((clds)->gc_list->goal == 0))	\
	{							\
	    Buf_Alloc(procedure->codebuf, code, L_GC);		\
	    Store_3d(Gc_testA, Esize((clds)->gc_list->size), arity);\
	}							\
    }

#define Gc_TestA_Next(clds)					\
    {								\
	if ((clds)->gc_list && ((clds)->gc_list->goal == 0))	\
	{							\
	    (clds)->gc_list = (clds)->gc_list->next;		\
	}							\
    }

#define Call_(c, det)	{if (det) {Store_d(c)} else {Store_d((c) + 2)}}
#define Jmps_(Gproc, Gval, procedure, determinate, module, debug, size, tool, cortn, willwake, dfid, ignore, gflags)	\
		if (determinate && DirectLink(Gval, procedure, Gproc))\
		{		/* Branch */				\
		    Debug_Call(Gproc, CALL_PORT|LAST_CALL, debug, gflags)\
		    if (size)					\
		    {						\
			    Store_2d(JmpdAs, Esize(size));	\
		    }						\
		    else						\
		    {						\
			    Store_d(JmpdA);				\
		    }						\
		    Set_Label(procedure->start)			\
		}							\
		else						\
		{							\
		    if (ignore) {					\
			Space_(size);				\
			Debug_For_Call(Gval, module, CALL_PORT|LAST_CALL, debug, gflags)	\
			Ret_(Gval, module, determinate, cortn, willwake, debug)	\
		    } else {					\
			Space_(size)				\
			willwake = 0;				\
			if (determinate) {				\
			    Store_Last_Goal(JmpdA, JmpdP, Gproc, Gval,	\
				module, CALL_PORT|LAST_CALL, debug, gflags)	\
			} else {					\
			    Store_Last_Goal(JmpA, JmpP, Gproc, Gval,	\
				module, CALL_PORT|LAST_CALL, debug, gflags)	\
			}						\
		    }						\
		}
#define Ret_(did, module, determinate, cortn, willwake, debug)	\
			willwake = 0;			\
			Debug_Exit(debug);		\
			if (determinate)		\
				{Store_d(Retd)}		\
			else				\
				{Store_d(Ret)}
#define Retd_(did, module, cortn, willwake, debug)		\
			willwake = 0;			\
			Debug_Exit(debug);		\
			Store_d(Retd)

#define Chain_(Gproc, Gval, module, cortn, willwake, debug, ignore, gflags) \
		    if (ignore) {					\
			Exit_(cortn, willwake, debug)			\
		    } else {						\
			Store_Last_Goal(ChainA, ChainP, Gproc, Gval,	\
				module, CALL_PORT|LAST_CALL, debug, gflags)	\
		    }
#define Chaind_(Gproc, Gval, module, cortn, willwake, debug, ignore, gflags) \
		    if (ignore) {					\
			Exitd_(cortn, willwake, debug)			\
		    } else {						\
			Store_Last_Goal(ChaindA, ChaindP, Gproc, Gval,	\
				module, CALL_PORT|LAST_CALL, debug, gflags)	\
		    }
#define Chainc_(Gproc, Gval, module, cortn, willwake, debug, ignore, gflags) \
		    if (ignore) {					\
			Exitc_(cortn, willwake, debug)			\
		    } else {						\
			Debug_Cut(d_.cut, d_.kernel_sepia, CUT_PORT, debug)\
			Store_Last_Goal(ChaincA, ChaincP, Gproc, Gval,	\
				module, CALL_PORT|LAST_CALL, debug, gflags)	\
		    }

#define Exit_(cortn, willwake, debug)		\
				willwake = 0;		\
				Debug_Exit(debug);	\
				Store_d(Exit);
#define Exitc_(cortn, willwake, debug)			\
				willwake = 0;		\
				Debug_Cut(d_.cut, d_.kernel_sepia, CUT_PORT|LAST_CALL, debug);\
				Store_d(Exitc);
#define Exitd_(cortn, willwake, debug)			\
				willwake = 0;		\
				Debug_Exit(debug);	\
				Store_d(Exitd)

#define Branch_(label)		Store_d(Branch); label = code++;

#define Escape_(gproc, goal, gval, clds, procedure, willwake, gflags, state) \
		code = ec_escape(gproc, goal, gval, clds, procedure, code,	\
			WDisj(gflags, state), gflags);			\
		if ((clds->cflags & (CL_VARS | CL_MAYWAKE)) >=		\
		    (CL_WAKE | CL_MAYWAKE))				\
		    willwake = 1;					\
		clds->cflags &= ~(CL_VARS | CL_MAYWAKE);

#define Output_Mode(clds, procedure, code, where, head_pass, willwake)	\
		if (head_pass[OUTPUT_NONVAR])				\
		{							\
		    Reset_Clause_Willwake(clds);			\
		    code = output_mode(clds, procedure, code, where);\
		    if (ClauseWillwake(clds))				\
			willwake = 1;					\
		}
#define Cut_(env_size, debug, gflags, nested_cut)			\
    code = _cut(env_size, nested_cut, gflags, debug, code);\
    if (env_size < 1) _codegen_error(procedure, clause_number, "cut variable trimmed");

#define Neck_(shallow, cut, arity)					\
		if(cut)							\
		{							\
		    Store_d(Savecut);					\
		}
#define Neckcut_(shallow, determinate, cut, debug)			\
		if(cut)							\
		{							\
		    Store_d(Savecut);					\
		}							\
		if (!determinate && !GNested(gflags))			\
		{							\
		    Debug_Cut(d_.cut, d_.kernel_sepia, CUT_PORT, debug)\
		    Store_d(Neckcut)					\
		}

#define Res_(willwake, arity, tool, envsize)				\
		if (willwake) {						\
			Store_3d(Res, tool ? arity + 1 : arity, Esize(envsize))\
		}
#define Ress_(willwake, size, arity, tool, envsize)			\
		if (willwake) {						\
		    if (size) {						\
			Store_4d(Ress, Esize(size), tool ? arity + 1 : arity,\
				Esize(envsize))\
		    }							\
		    else {						\
			Store_3d(Res, tool ? arity + 1 : arity, Esize(envsize))\
		    }							\
		}
#define Space_Res(size, arity, tool, willwake, envsize)		\
			if (willwake)					\
			{						\
				Ress_(willwake, size, arity, tool, envsize);\
			}						\
			else						\
			{						\
				Space_(size);				\
			}

/*
 * Debug port generation for old debugger, now obsolete
 */

#define Debug_Port(Port, debug)
#define Debug_Exit(debug)
#define Debug_(Did, Module, Port, debug)
#define Debug_Cut(Did, Module, Port, debug)
#define Debug_For_Call(Did, Module, Port, debug, gflags)
#define Debug_Port_For_Call(Port, debug, gflags)

#define Debug_Call(Proc, Port, debug, gflags) \
			    if (debug) {\
				int	__port = (Port);\
				Store_2d(Debug_call, Proc)\
				if (GFirst(gflags))\
				    __port |= FIRST_CALL;\
				if (GLast(gflags) || GLastn(gflags))\
				    __port |= LAST_CALL;\
				Store_d(__port)\
			    }

#ifdef OC
#define Occur_Check_Next(m)	\
		if (GlobalFlags & OCCUR_CHECK && !NoaliasMode(m)) \
		    {Store_d(Occur_check_next)}
#else
#define Occur_Check_Next(m)
#endif

/* GET INSTRUCTIONS */

#define Get_VariableNL(size, tvv, i)				\
				Store_Rev_Arg_3d(		\
					Get_variableNAML,	\
					Get_variableNARL,	\
					Esize(size),		\
					i,			\
					VarPermOff(tvv))
#define Get_VariableL(tvv, i)	Store_Arg_2d(MoveAML, MoveARL, i, VarPermOff(tvv))
#define Get_VariableT(temp, p, i)				\
				Store_First_Arg_Temp(		\
					MoveAM,			\
					MoveAMTR,		\
					MoveAR,			\
					MoveARTR,		\
					i,			\
					temp,			\
					p)
#define Get_ValueL(tvv, i, delcl)				\
			if (delcl)				\
			{					\
				Store_Arg_2d(			\
					Get_matched_valueAML,	\
					Get_matched_valueARL,	\
					i, VarPermOff(tvv))	\
			}					\
			else					\
			{					\
				Store_Arg_2d(			\
					Get_valueAML,		\
					Get_valueARL,		\
					i, VarPermOff(tvv))	\
			}
#define Get_ValueT(temp, p, i, delcl)				\
			if (delcl)				\
			{					\
				Store_Arg_Temp(			\
					Get_matched_valueAMTM,	\
					Get_matched_valueAMTR,	\
					Get_matched_valueARTM,	\
					Get_matched_valueARTR,	\
					i, temp, p)		\
			}					\
			else					\
			{					\
				Store_Arg_Temp(			\
					Get_valueAMTM,		\
					Get_valueAMTR,		\
					Get_valueARTM,		\
					Get_valueARTR,		\
					i, temp, p)		\
			}
#define Get_ValueA(i, j, delcl)					\
			if (delcl)				\
			{					\
			    Store_Two_Args(			\
				Get_matched_valueAMAM,		\
				Get_matched_valueAMAR,		\
				Get_matched_valueAMAR,		\
				Inst_Error,			\
				i, j)				\
			}					\
			else					\
			{					\
			    Store_Two_Args(			\
				Get_valueAMAM,			\
				Get_valueAMAR,			\
				Get_valueAMAR,			\
				Inst_Error,			\
				i, j)				\
			}
#define Get_Constant(tag, val, i, mode)				\
				if (InputMode(mode))		\
				{				\
					Store_Arg_3d(		\
						In_get_constantAM,\
						In_get_constantAR,\
						i,		\
						val,		\
						tag)		\
				}				\
				else if (mode == OUTPUT)	\
				{				\
					Out_Get_Constant(tag, val, i)\
				}				\
				else				\
				{				\
					Store_Arg_3d(		\
						Get_constantAM,	\
						Get_constantAR,	\
						i,		\
						val,		\
						tag)		\
				}

#define Get_Atom(val, i, mode)					\
				if (InputMode(mode))		\
				{				\
					Store_Arg_2d(		\
						In_get_atomAM,	\
						In_get_atomAR,	\
						i,val)		\
				}				\
				else if (mode == OUTPUT)	\
				{				\
					Out_Get_Atom(val, i)	\
				}				\
				else				\
				{				\
					Store_Arg_2d(		\
						Get_atomAM,	\
						Get_atomAR,	\
						i,val)		\
				}

#define Get_Integer(val, i, mode)				\
				if (InputMode(mode))		\
				{				\
					Store_Arg_2d(		\
						In_get_integerAM,\
						In_get_integerAR,\
						i,val)		\
				}				\
				else if (mode == OUTPUT)	\
				{				\
					Out_Get_Integer(val, i)	\
				}				\
				else				\
				{				\
					Store_Arg_2d(		\
						Get_integerAM,	\
						Get_integerAR,	\
						i,val)		\
				}
#define Get_Float(val, i, mode)					\
				if (InputMode(mode))		\
				{				\
					Store_Arg_2d(		\
						In_get_floatAM,	\
						In_get_floatAR,	\
						i,val)		\
				}				\
				else if (mode == OUTPUT)	\
				{				\
					Out_Get_Float(val, i)	\
				}				\
				else				\
				{				\
					Store_Arg_2d(		\
						Get_floatAM,	\
						Get_floatAR,	\
						i,val)		\
				}

#define Get_Nil(i, mode)					\
				if (InputMode(mode))		\
				{				\
					Store_Arg_1d(		\
						In_get_nilAM,	\
						In_get_nilAR,	\
						i)		\
				}				\
				else if (mode == OUTPUT)	\
				{				\
					Out_Get_Nil(i)		\
				}				\
				else				\
				{				\
					Store_Arg_1d(		\
						Get_nilAM,	\
						Get_nilAR,	\
						i)		\
				}

#define Get_String(val, i, mode)				\
				if (InputMode(mode))		\
				{				\
					Store_Arg_2d(		\
						In_get_stringAM,\
						In_get_stringAR,\
						i,val)		\
				}				\
				else if (mode == OUTPUT)	\
				{				\
					Out_Get_String(val, i)	\
				}				\
				else				\
				{				\
					Store_Arg_2d(		\
						Get_stringAM,	\
						Get_stringAR,	\
						i,val)		\
				}

#define Get_Structure(d, i, mode)				\
				if (InputMode(mode))		\
				{				\
					Store_Arg_2d(		\
						In_get_structureAM,\
						In_get_structureAR,\
						i, d)		\
				}				\
				else if (mode == OUTPUT)	\
				{				\
					Out_Get_Structure(d, i)\
				}				\
				else				\
				{				\
					Store_Arg_2d(		\
						Get_structureAM,\
						Get_structureAR,\
						i, d)		\
				}

#define Get_List(i, mode)					\
				if (InputMode(mode))		\
				{				\
					Store_Arg_1d(		\
						In_get_listAM,	\
						In_get_listAR,	\
						i)		\
				}				\
				else if (mode == OUTPUT)	\
				{				\
					Out_Get_List(i)		\
				}				\
				else				\
				{				\
					Store_Arg_1d(		\
						Get_listAM,	\
						Get_listAR,	\
						i)		\
				}


#define Out_Get_Constant(tag,val,i)    \
        Store_Arg_3d(Out_get_constantAM,Out_get_constantAR,i,val,tag)
#define Out_Get_Atom(val, i)				\
				Store_Arg_2d(			\
					Out_get_atomAM,	\
					Out_get_atomAR,	\
					i,val)
#define Out_Get_Integer(val, i)					\
				Store_Arg_2d(			\
					Out_get_integerAM,	\
					Out_get_integerAR,	\
					i,val)
#define Out_Get_Float(val, i)					\
				Store_Arg_2d(			\
					Out_get_floatAM,	\
					Out_get_floatAR,	\
					i,val)
#define Out_Get_String(val, i)					\
				Store_Arg_2d(			\
					Out_get_stringAM,	\
					Out_get_stringAR,	\
					i,val)
#define Out_Get_Nil(i)						\
				Store_Arg_1d(			\
					Out_get_nilAM,		\
					Out_get_nilAR,		\
					i)
#define Out_Get_Structure(d, i)					\
      				Store_Arg_2d(			\
					Out_get_structureAM,	\
					Out_get_structureAR,	\
					i, d)
#define Out_Get_List(i)						\
				Store_Arg_1d(			\
					Out_get_listAM,		\
					Out_get_listAR,		\
					i)
#define Get_Structure_Arguments(i)				\
				Store_Arg_1d(			\
					Get_structure_argumentsAM,\
					Get_structure_argumentsAR,\
					i)
#define Get_List_Arguments(i)	Store_Arg_1d(			\
					Get_list_argumentsAM,	\
					Get_list_argumentsAR,	\
					i)

/* WRITE INSTRUCTIONS (READ are only in the code) */

#define Write_Void(tvv)		\
    {if (IsName(tvv->header.tag)) \
	{Store_2d(Write_named_void, \
	    tvv->header.tag.kernel)}\
    else if (IsMeta(tvv->header.tag)) \
	{Store_d(Read_void); in_meta = 0;}\
    else\
	{Store_d(Write_void)}\
    }
#define Write_VariableNL(size, tvv)	\
    {if (IsName(tvv->header.tag)) \
	{Store_4d(Write_named_variableNL, Esize(size), VarPermOff(tvv), \
	    tvv->header.tag.kernel)}\
    else if (IsMeta(tvv->header.tag))\
	{Store_3d(Read_referenceNL, Esize(size), VarPermOff(tvv)); in_meta = 0;}\
    else\
	{Store_3d(Write_variableNL, Esize(size), VarPermOff(tvv))}\
    }
#define Write_VariableL(tvv)	\
    {if (IsName(tvv->header.tag)) \
	{Store_3d(Write_named_variableL, VarPermOff(tvv), \
	    tvv->header.tag.kernel)}\
    else if (IsMeta(tvv->header.tag)) \
	{Store_2d(Read_referenceL, VarPermOff(tvv)); in_meta = 0;} \
    else\
	{Store_2d(Write_variableL, VarPermOff(tvv))}\
    }
#define Write_VariableA(i, tvv)	\
    {if (IsName(tvv->header.tag)) \
	{Store_Arg_2d(Write_named_variableAM, Write_named_variableAR, i, \
	    tvv->header.tag.kernel)}\
    else if (IsMeta(tvv->header.tag)) \
	{Store_2d(Read_referenceAM, Address(i)); in_meta = 0;} \
    else\
	{Store_Arg_1d(Write_variableAM, Write_variableAR, i)}\
    }
#define Write_VariableT(tmp, tvv) \
    {if (IsName(tvv->header.tag)) \
	{Store_First_Temp_2d(Write_named_variable, Write_named_variableTR, tmp,\
	    tvv->header.tag.kernel)}\
    else if (IsMeta(tvv->header.tag)) \
	{Store_d(Read_reference); in_meta = 0;} \
    else\
	{Store_d(Write_variable)}\
    }
#define Write_ValueL(tvv)	\
    {if (IsMeta(tvv->header.tag) && in_meta) \
	{Store_2d(Read_valueL, VarPermOff(tvv))} \
    else\
	{Store_2d(Write_valueL, VarPermOff(tvv))}\
    }
#define Write_Local_ValueL(tvv)	Store_2d(Write_local_valueL, VarPermOff(tvv))
#define Write_ValueA(i)		\
    {if (IsMeta(tvv->header.tag) && in_meta) \
	{Store_2d(Read_valueAM, Address(i))} \
    else\
	{Store_Arg_1d(Write_valueAM, Write_valueAR, i)}\
    }
#define Write_Local_ValueA(i)	Store_Arg_1d(Write_local_valueAM, Write_local_valueAR, i)
#define Write_ValueT(tmp, p)	\
    {if (IsMeta(tvv->header.tag) && in_meta) \
	{Store_2d(Read_valueTM, TempOff(tmp, p))} \
    else\
	{Store_Temp_1d(Write_valueTM, Write_valueTR, tmp, p)}\
    }
#define Write_Local_ValueT(tmp, p)	\
		Store_Temp_1d(Write_local_valueTM, Write_local_valueTR, tmp, p)

#define Write_Constant(tag,val) Store_3d(Write_constant,val,tag)
#define Write_Atom(val)		Store_2d(Write_did, val)
#define Write_Integer(val)	Store_2d(Write_integer, val)
#define Write_Float(val)	Store_2d(Write_float, val)
#define Write_Nil		Store_d(Write_nil)
#define Write_String(val)	Store_2d(Write_string, val)
#define Write_Structure(attr, did)	\
	{if (attr) {				\
		Store_2d(Read_attribute, 0)	\
	} else {				\
		Store_2d(Write_structure, did)	\
	}}
#define Write_List(str)		Store_d(Write_list)

#define First_(temp)		Store_First_Temp_1d(First, FirstTR, temp)
#define Mode2_(temp, p, m, l)	\
			if (OutputMode(m)) {\
			    Store_Temp_1d(ModeTM, ModeTR, temp, p)\
			} else {\
			    Store_Temp_2d(ModeTMlab, ModeTRlab, temp, p, l)\
			}
#define Next1_(temp, p)		Store_Temp_1d(NextTM, NextTR, temp, p)
#define Next2_(temp, p, l)	Store_Temp_2d(NextTMlab, NextTRlab, temp, p, l)

/* PUT INSTRUCTIONS */

#define Put_Variable(tvv, i)	\
    {if (IsName(tvv->header.tag)) {\
	Store_4d(Put_referenceAM, Address(i), Esize(1), tvv->header.tag.kernel)\
	clds->s = clds->global++;\
    } else if (IsMeta(tvv->header.tag)) {\
	Store_4d(Put_referenceAM, Address(i), Esize(2),\
	    tvv->header.tag.kernel)\
	Store_d(Read_void);\
	clds->s = clds->global + 1;\
	clds->global += 2;\
    } else {\
	Store_Arg_1d(Put_variableAM, Put_variableAR, i)\
	clds->global++;\
    }\
    }
#define Put_VariableL(tvv, i)	\
    {if (IsName(tvv->header.tag)) {\
	Store_4d(Put_referenceAML, Address(i), VarPermOff(tvv), Esize(1))\
	Store_d(tvv->header.tag.kernel)\
	clds->s = clds->global++;\
    } else if (IsMeta(tvv->header.tag)) {\
	Store_4d(Put_referenceAML, Address(i), VarPermOff(tvv), Esize(2))\
	Store_d(tvv->header.tag.kernel)\
	clds->s = clds->global;\
	clds->global += 2;\
    } else\
	{Store_Arg_2d(Put_variableAML, Put_variableARL, i, VarPermOff(tvv))}\
    }
#define Put_ValueL(tvv, i)	\
	{Store_Rev_Arg_2d(MoveLAM, MoveLAR, VarPermOff(tvv), i)}
#define Put_Unsafe_ValueL(tvv, i)				\
				Store_Arg_2d(			\
					Put_unsafe_valueAML,	\
					Put_unsafe_valueARL,	\
					i, VarPermOff(tvv))
#define Put_ValueA(i, j)	\
		    Store_Two_Args(MoveAMAM, MoveAMAR, MoveARAM, Inst_Error,\
			i, j)
/*
#define Put_Unsafe_ValueA(i, j)					\
				Store_Two_Args(			\
					Put_unsafe_valueAMAM,	\
					Put_unsafe_valueAMAR,	\
					Put_unsafe_valueARAM,	\
					Inst_Error,		\
					i, j)
*/
#define Put_ValueT(temp, p, i)	\
	Store_Temp_Arg(MoveTMAM, MoveTRAM, MoveTMAR, MoveTRAR, temp, p, i)

#define Put_Unsafe_ValueT(temp, p, i)	\
	Store_Arg_Temp(Put_unsafe_valueAMTM, Inst_Error, Put_unsafe_valueARTM, Inst_Error, i, temp, p)

#define Put_Constant(tag,val,arg)    \
        Store_Arg_3d(Put_constantAM,Put_constantAR,arg,tag,val)
#define Put_Atom(val, i)	Store_Arg_2d(Put_atomAM, Put_atomAR, i, val)
#define Put_Integer(val, i)	\
			Store_Arg_2d(Put_integerAM, Put_integerAR, i, val)
#define Put_Float(val, i)	Store_Arg_2d(Put_floatAM, Put_floatAR, i, val)
#define Put_Nil(i)		Store_Arg_1d(Put_nilAM, Put_nilAR, i)
#define Put_String(val, i)	Store_Arg_2d(Put_stringAM, Put_stringAR, i, val)
#define Put_Structure(wd, i)	\
			Store_Arg_2d(Put_structureAM, Put_structureAR, i, wd)\
			clds->s = clds->global + 1;\
			clds->global += DidArity(wd) + 1;
#define Put_List(i)		Store_Arg_1d(Put_listAM, Put_listAR, i)\
				clds->s = clds->global;\
				clds->global += 2;

/* PUTS INSTRUCTIONS */

#define Puts_VariableL(tvv)	\
    {if (IsName(tvv->header.tag)) {\
	Store_4d(Puts_referenceL, VarPermOff(tvv), Esize(1),\
	    tvv->header.tag.kernel)\
	clds->s = clds->global++;\
    } else if (IsMeta(tvv->header.tag)) {\
	Store_4d(Puts_referenceL, VarPermOff(tvv), Esize(2),\
	    tvv->header.tag.kernel)\
	clds->s = clds->global;\
	clds->global += 2;\
    } else\
	{Store_2d(Puts_variableL, VarPermOff(tvv))}\
    }
#define Puts_Variable(tvv)	\
    {if (IsName(tvv->header.tag)) {\
	Store_3d(Puts_reference, Esize(1), tvv->header.tag.kernel)\
	clds->s = clds->global++;\
    } else if (IsMeta(tvv->header.tag)) {\
	Store_3d(Puts_reference, Esize(2), tvv->header.tag.kernel)\
	Store_d(Read_void)\
	clds->s = clds->global + 1;\
	clds->global += 2;\
    } else\
	{Store_d(Puts_variable)}\
    }
#define Puts_VariableG(tvv, clds)	\
    {if (IsName(tvv->header.tag)) {\
	Store_3d(Puts_reference, Esize(1), tvv->header.tag.kernel)\
	Set_Var_Source(tvv, ContGl(clds->global));\
	clds->s = clds->global++;\
    } else if (IsMeta(tvv->header.tag)) {\
	Store_3d(Puts_reference, Esize(2), tvv->header.tag.kernel)\
	Store_d(Read_void)\
	Set_Var_Source(tvv, ContGl(clds->global));\
	clds->s = clds->global + 1;\
	clds->global += 2;\
    } else\
	{Store_d(Puts_variable)}\
    }
#define Puts_ValueL(tvv)	Store_2d(Puts_valueL, VarPermOff(tvv))
#define Puts_ValueA(i)		Store_Arg_1d(Puts_valueAM, Puts_valueAR, i)
#define Puts_ValueT(tmp, p)	Store_2d(Puts_valueTM, TempOff(tmp, p))
#define Puts_ValueG(tmp, s)	Store_2d(Puts_valueG, GTempOff(tmp, s))
#define Puts_Constant(tag,val)  Store_3d(Puts_constant,tag,val)
#define Puts_Atom(val)		Store_2d(Puts_atom, val)
#define Puts_Integer(val)	Store_2d(Puts_integer, val)
#define Puts_Proc(val)		Store_2d(Puts_proc, val)
#define Puts_Float(val)		Store_2d(Puts_float, val)
#define Puts_Nil		Store_d(Puts_nil)
#define Puts_String(val)	Store_2d(Puts_string, val)
#define Puts_List()		Store_d(Puts_list);\
				clds->s = clds->global;\
				clds->global += 2;
#define Puts_Structure(wd)	Store_2d(Puts_structure, wd)\
				clds->s = clds->global + 1;\
				clds->global += DidArity(wd) + 1;

/* PUSH INSTRUCTIONS */

#define Push_Void(tvv)		\
    {if (IsName(tvv->header.tag)) {\
	Store_2d(Push_void_reference, Esize(1));\
	enqueue = 1;\
	clds->global++;\
    } else if (IsMeta(tvv->header.tag)) {\
	Store_2d(Push_void_reference, Esize(2));\
	enqueue = 1;\
	clds->global += 2;\
    } else\
	{Store_d(Write_void)} \
    }
#define Push_VariableL(tvv)	\
    {if (IsName(tvv->header.tag)) {\
	Store_3d(Push_referenceL, VarPermOff(tvv), Esize(1));\
	enqueue = 1;\
	clds->global += 1;\
    } else if (IsMeta(tvv->header.tag)) {\
	Store_3d(Push_referenceL, VarPermOff(tvv), Esize(2));\
	enqueue = 1;\
	clds->global += 2;\
    } else\
	{Store_2d(Push_variableL, VarPermOff(tvv))}\
    }
#define Push_VariableA(i, tvv)	\
    {if (IsName(tvv->header.tag)) \
	{Store_3d(Push_referenceAM, Address(i), Esize(1));\
	enqueue = 1;\
	clds->global++;}\
    else if (IsMeta(tvv->header.tag)) \
	{Store_3d(Push_referenceAM, Address(i), Esize(2)); enqueue = 1;\
	clds->global += 2;}\
    else\
	{Store_Arg_1d(Write_variableAM, Push_variableAR, i)}\
    }
#define Push_VariableT(tmp, tvv)	\
    {if (IsName(tvv->header.tag)) {\
	Store_2d(Push_reference, Esize(1)); enqueue = 1;\
	clds->global++;\
    } else if (IsMeta(tvv->header.tag)) {\
	Store_2d(Push_reference, Esize(2)); enqueue = 1;\
	clds->global += 2;\
    } else {\
	Store_First_Temp_1d(Write_variable, Push_variableTR, tmp)\
    }}
#define Push_VariableG(tvv, clds)	\
    {if (IsName(tvv->header.tag)) {\
	Store_2d(Push_void_reference, Esize(1));\
	enqueue = 1;\
	Set_Var_Source(tvv, ContGl(clds->global));\
	clds->global++;\
    } else if (IsMeta(tvv->header.tag)) {\
	Store_2d(Push_void_reference, Esize(2));\
	enqueue = 1;\
	Set_Var_Source(tvv, ContGl(clds->global));\
	clds->global += 2;\
    } else {\
	Store_d(Write_void)\
	Set_Var_Source(tvv, ContGl(clds->s));\
    }}
#define Push_Init_VariableL(tvv)	\
    {if (IsMeta(tvv->header.tag)) {\
	Store_3d(Push_init_referenceL, VarPermOff(tvv), Esize(2));\
	enqueue = 1;\
	clds->global += 2;\
    } else if (IsName(tvv->header.tag)) \
	{Push_ValueL(tvv)}\
    else\
	{Store_2d(Push_init_variableL, VarPermOff(tvv))}\
    }
#define Push_Named_Variable(tvv) \
	Store_2d(Push_self_reference, (tvv)->header.tag.kernel)
#define Push_ValueL(tvv)	Store_2d(Write_valueL, VarPermOff(tvv))
#define Push_ValueA(i)		Store_Arg_1d(Write_valueAM, Push_valueAR, i)
#define Push_ValueT(temp, p)	Store_2d(Write_valueTM, TempOff(temp, p))
#define Push_ValueG(temp, s)	Store_2d(Push_valueG, GTempOff(temp, s))
#define Push_Local_ValueT(temp, p)	\
			Store_Temp_1d(Push_local_valueTM, Push_local_valueTR, temp, p)
#define Push_Local_ValueA(var)	\
			Store_Arg_1d(Push_local_valueAM, Push_local_valueAR, var)
#define Push_Local_ValueL(tvv)	Store_2d(Push_local_valueL, VarPermOff(tvv))
#define Push_Constant(tag,val)  Store_3d(Write_constant,val,tag)
#define Push_Atom(val)		Store_2d(Write_did, val)
#define Push_Integer(val)	Store_2d(Write_integer, val)
#define Push_Float(val)		Store_2d(Write_float, val)
#define Push_Nil		Store_d(Write_nil)
#define Push_Functor(d)		Store_2d(Write_did, d)
#define Push_String(val)	Store_2d(Write_string, val)
#define Push_List(str)		Store_d(Push_list); clds->global += 2;
#define Push_Structure(arity)	Store_2d(Push_structure, (Esize(arity + 1)))\
				clds->global += arity + 1;

/* DEFAULTS for the extensions */
/* do the following need 'code' ? */
#define Puts_Default(tag, val)
#define Read_Default(wptr)		default:\
	    p_fprintf(current_err_,\
		"wrong opcode (%d) in the write sequence\n", *(wptr));\
	    exit(-1);

/* INDEXING and CHOICE POINT INSTRUCTIONS */

#define Try_Me_Else(arity, label, debug)			\
		Store_3d(Try_me_else, NO_PORT, arity)		\
		Forward_Label(label)
#define Retry_Me_Else(label, debug)				\
		Store_2d(Retry_me_else, debug?NEXT_PORT:NO_PORT) \
		Forward_Label(label)
#define Trust_Me(label, debug)					\
		Store_2d(Trust_me, debug?NEXT_PORT:NO_PORT)

#define Try_(arity, label, debug)				\
		Store_3d(Try, NO_PORT, arity)			\
		Set_Label(label)
#define Retry_(label, debug)					\
		Store_2d(Retry, debug?NEXT_PORT:NO_PORT)	\
		Set_Label(label)
#define Trust_(label, debug)					\
		Store_2d(Trust, debug?NEXT_PORT:NO_PORT)	\
		Set_Label(label)

#define Try_Else(arity, lab1, lab2, debug)			\
		Store_3d(Trylab, NO_PORT, arity)		\
		Set_Label(lab1)					\
		Set_Label(lab2)
#define Retry_Else(lab1, lab2, debug)				\
		Store_2d(Retrylab, debug?NEXT_PORT:NO_PORT)	\
		Set_Label(lab1)					\
		Set_Label(lab2)

#define Retry_Me_Inline(label, debug, env_size)			\
		Store_2d(Retry_me_inline, INLINE_PORT|(debug?ELSE_PORT:NO_PORT))	\
		Forward_Label(label)				\
		Store_d(Esize(env_size))
#define Trust_Me_Inline(label, debug, env_size)			\
		Store_2d(Trust_me_inline, INLINE_PORT|(debug?ELSE_PORT:NO_PORT))	\
		Store_d(Esize(env_size))

#define List_Switch(i)		\
			Store_Arg_1d(List_switchAM, List_switchAR, i); code += 3;
#define Functor_Switch(i)						\
			Store_Arg_1d(					\
				Functor_switchAM,			\
				Functor_switchAR,			\
				i);					\
			code += 3;
#define Functor_Switch_Seq(i)						\
			Store_Arg_1d(					\
				Functor_switch_seqAM,			\
				Functor_switch_seqAR,			\
				i);					\
			code += 2;
#define Atom_Switch(i)							\
			Store_Arg_1d(					\
				Atom_switchAM,				\
				Atom_switchAR,				\
				i);					\
			code += 3;
#define Atom_Switch_Seq(i)						\
			Store_Arg_1d(					\
				Atom_switch_seqAM,			\
				Atom_switch_seqAR,			\
				i);					\
			code += 2;
#define Integer_Switch(i)						\
			Store_Arg_1d(					\
				Integer_switchAM,			\
				Integer_switchAR,			\
				i);					\
			code += 3;
#define Integer_Switch_Seq(i)						\
			Store_Arg_1d(					\
				Integer_switch_seqAM,			\
				Integer_switch_seqAR,			\
				i);					\
			code += 2;

#define Switch_On_Type(i)						\
			Store_Arg_1d(					\
				Switch_on_typeAM,			\
				Switch_on_typeAR,			\
				i);					\
			code += NTYPES;

/*
 * EXTERNAL VARIABLE DECLARATIONS: 
 */
extern vmcode	fail_code_[];
