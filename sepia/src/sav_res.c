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
 * SEPIA C SOURCE MODULE
 *
 * VERSION	$Id: sav_res.c,v 1.1 2006/09/23 01:56:16 snovello Exp $
 */

/*
 * IDENTIFICATION               sav_res.c
 *
 * DESCRIPTION                  The SEPIA save and restore.
 *
 * CONTENTS:
 *
 * AUTHOR               VERSION  DATE   REASON
 * Emmanuel van Rossum                  created the file
 * Joachim Schimpf		10/93	modified to be machine independent
 */

#include <stdio.h>
#include "config.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef PATH_IN_LIMITS
#  include 	<limits.h>
#  define MAX_PATH_LEN	PATH_MAX
#else
#  include <sys/param.h>
#  define MAX_PATH_LEN	MAXPATHLEN
#endif

#include <errno.h>

#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "mem.h"
#include "error.h"
#include "dict.h"
#include "emu_export.h"
#include "io.h"
#include "property.h"
#include "sav_res.h"

/*-----------------------------------------------------------------------
 * Macros
 *-----------------------------------------------------------------------*/

#define	SEPIA_SHELL		"/bin/sh"
#define SAVED_STATE_MAGIC	0xecececec
#define SAVED_STATE_VERSION	4

#define Check_Error(err)					\
    if (err != PSUCCEED)					\
    {								\
	Bip_Error(err)						\
    }

#define Check_Read_Error(res, n, fd) {				\
	if (res != n) {						\
	    (void) close(fd);					\
	    if (res == -1) { Set_Errno; Bip_Error(SYS_ERROR); }	\
	    else { Bip_Error(UNEXPECTED_EOF); }			\
	}							\
}


/* ***************************************************************** */
extern		end;

extern char	*strcpy(),
		*strcat();

extern		io_init();
extern void	flush_and_close_io(),
		set_string();
extern char	*getenv(),
		*expand_filename();

extern jmp_buf	reset;
extern unsigned	ec_vers;
extern uword	*start_of_stacks,
		*end_of_stacks;

int		p_save_program();

/*-----------------------------------------------------------------------
 * Format of a saved state file:
 *
 *	ss_header
 *	one or more data_blocks
 *	empty data_block
 *
 * where a data_block consists of a ss_block_header followed by the data
 *-----------------------------------------------------------------------*/

typedef struct s_ss_header
{
    char	auto_exec[MAX_PATH_LEN + 128]; /* to store exec sepia	*/
    int		magic; 		/* magic number				*/
    int		version;	/* format version identifier		*/
    int		sstype;		/* type of saved state			*/
    int		layout;		/* type of memory layout		*/
    char *	engine;		/* value of &g_emu_ in the saving sepia	*/
				/* used for consistency check only	*/
    char *	s_start;	/* start of stacks when saving		*/
} ss_header;

struct ss_block_header {
	char	*addr;		/* main memory address of this block	*/
	int	len;		/* number of data bytes in this block	*/
};

/* ***************************************************************** */
void
save_res_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	/* (moved to emu_util.c)
	(void) built_in(in_dict("save", 1),BISave, B_EXPANDED);
	(void) built_in(in_dict("restore", 1),BIRestore, B_EXPANDED);
	*/
	(void) built_in(in_dict("save_program",1),p_save_program,B_UNSAFE);
    }
}

/* ***************************************************************** */
static int
_create_saved_file(char *name, int *fd)
{
    int um;
    char buf[MAX_PATH_LEN];
    struct stat	sbuf;
#ifdef O_BINARY
    int oflags = O_WRONLY | O_CREAT | O_TRUNC | O_BINARY;
#else
    int oflags = O_WRONLY | O_CREAT | O_TRUNC;
#endif
    
    um = umask(777);
    (void) umask(um);
    name = expand_filename(name, buf);
    if ((*fd = ec_open(name, oflags, 0777 & ~um)) < 0)
    {
	Set_Errno;
	Bip_Error(SYS_ERROR);
    }
    (void) fstat(*fd, &sbuf);
    if ((sbuf.st_mode & 0100) == 0)
	(void) chmod(name, 0777 & ~um);
    Succeed_;
}

static char *
_restore_shell(void)
{
    char	*s;

    s = getenv("ECLIPSESHELL");
    return s ? s : SEPIA_SHELL;
}

/*
 * The header of a saved memory block: address and length in bytes.
 * When address is NULL, this is the last block in the file.
 */

static int		/* PSUCCEED if ok, PFAIL on end, Error else */
_read_block(int fd)
{
    struct ss_block_header hdr;
    int res;

    res = read(fd, (char *) &hdr, sizeof(struct ss_block_header));
    Check_Read_Error(res, sizeof(struct ss_block_header), fd)

    if (!hdr.addr) { Fail_ }	/* terminator block */

    res = read(fd, hdr.addr, hdr.len);
    Check_Read_Error(res, hdr.len, fd)

    Succeed_;
}

static int
_write_block(int fd, char *from, char *to)
{
    struct ss_block_header hdr;
    hdr.addr = from;
    hdr.len = to - from;
    if (write(fd, (char *) &hdr, sizeof(struct ss_block_header)) < 0
     || (hdr.addr && (write(fd, hdr.addr, hdr.len) < 0)))
    {
	(void) close(fd);
	Set_Errno;
	Bip_Error(SYS_ERROR);
    }
    Succeed_;
}

/* ***************************************************************** */
static int
_save_state(int fd, int what)
{
    int		err;
    ss_header	header;
    unsigned	saved_vers = ec_vers;
    char	saved_whoami[MAX_PATH_LEN];
    extern pword	*p_whoami_;
    extern pword	*p_binary_;

    /* the saved state cannot use the (maybe temporary) whoami */
    ec_vers = 0;
    /* save whoami */
    if (p_whoami_ && IsString(p_whoami_->tag))
	(void) strcpy(saved_whoami, StringStart(p_whoami_->val));
    else
	(void) strcpy(saved_whoami, *ec_options.Argv);

    free_heapterm(p_whoami_);
    (void) create_heapterm(p_whoami_, p_binary_->val, p_binary_->tag);

    header.layout = 0;

    (void) strcpy(header.auto_exec, "#! ");
    (void) strcat(header.auto_exec, _restore_shell());
    (void) strcat(header.auto_exec, "\nexec ");
    (void) strcat(header.auto_exec, *ec_options.Argv);
    header.magic = SAVED_STATE_MAGIC;
    header.version = SAVED_STATE_VERSION;
    header.sstype = what;
    header.engine = (char *) &g_emu_;
    header.s_start = (char *) start_of_stacks;
    if (what == PROG_ONLY)
    {
	(void) strcat(header.auto_exec,
	" -r $0 \"$@\"\n# ECLiPSe program saved state (unprintable)\n\n\n\0");

	/* stacks are not in the heap, they are mapped separately */
	header.sstype = REALLOCABLE;	/* this used to be conditional */
    }
    else	/* PROG_AND_DATA */
    {
	(void) strcat(header.auto_exec,
	" -r $0 \"$@\"\n# ECLiPSe execution saved state (unprintable)\n\n\n\0");
    }

    /* write the header						*/
    if (write(fd, (char *) &header, sizeof(ss_header)) < 0)
    {
	Set_Errno;
	Bip_Error(SYS_ERROR);
    }

    /* The saved state must have HEAP_READY unset (during restore) */
    GlobalFlags &= ~HEAP_READY;
    err = shared_mem_save(&global_heap, fd);
    GlobalFlags |= HEAP_READY;
    if (err < 0)
    {
	Set_Errno;
	Bip_Error(SYS_ERROR);
    }

    if (what == PROG_AND_DATA)			/* write the stacks */
    {
	err = _write_block(fd,  (char *) &g_emu_,
				(char *) &g_emu_ + sizeof(struct machine));
	Check_Error(err);
	err = _write_block(fd,  (char *) g_emu_.global_trail[0].start,
				(char *) TG);
	Check_Error(err);
	err = _write_block(fd,  (char *) TT,
				(char *) g_emu_.global_trail[1].start);
	Check_Error(err);
	err = _write_block(fd,  (char *) g_emu_.control_local[0].start,
				(char *) B.args);
	Check_Error(err);
	err = _write_block(fd,  (char *) SP,
				(char *) g_emu_.control_local[1].start);
	Check_Error(err);
    }

    err = _write_block(fd, (char *) 0, (char *) 0);	/* terminator */
    Check_Error(err);
    ec_vers = saved_vers;
    set_string(p_whoami_, saved_whoami);
    Succeed_;
}

/* ***************************************************************** */
static int
_restore_stacks(int fd, ss_header *p_header)
{
    int		err;
    
    if (p_header->sstype == PROG_AND_DATA
     || p_header->sstype == REALLOCABLE)	/* deallocate the stacks */
    {
	(void) adjust_stacks(g_emu_.global_trail,
		g_emu_.global_trail[0].start, g_emu_.global_trail[1].start, 0);
	(void) adjust_stacks(g_emu_.control_local,
		g_emu_.control_local[0].start, g_emu_.control_local[1].start, 0);
	end_of_stacks = start_of_stacks;
    }

    if (p_header->sstype == PROG_AND_DATA)
    {
	err = _read_block(fd);			/* read g_emu_ */
	Check_Error(err);
	PARSENV = (void_ptr) 0;
    }
    if (p_header->sstype == PROG_AND_DATA)	/* allocate the stacks	*/
    {
	start_of_stacks =
	g_emu_.global_trail[0].end = g_emu_.global_trail[0].start;
	g_emu_.global_trail[1].end = g_emu_.global_trail[1].start;
	g_emu_.control_local[0].end = g_emu_.control_local[0].start;
	end_of_stacks =
	g_emu_.control_local[1].end = g_emu_.control_local[1].start;
	if (!trim_global_trail(TG_SEG) || !trim_control_local())
	    ec_panic(MEMORY_P, "restore");
    }

    do						/* read the stacks	*/
	err = _read_block(fd);
    while (err == PSUCCEED);
    if (err != PFAIL)
	return err;
    Succeed_;
}

/* ***************************************************************** */
#if defined(HAVE_MMAP)
int
p_save(value v, type t)
{
#ifdef AS_EMU

    Bip_Error(NOT_IMPLEMENTED);

#else /* AS_EMU */

    int		fd;
    int		err;
    char *	name;

    Get_Name(v,t,name);

    /* check if no saving from another level than level 1	     */
    if (g_emu_.nesting_level > 1)
    {
	Bip_Error(WRONG_LEVEL);
    }
    err = _create_saved_file(name, &fd);
    Check_Error(err);
    err = _save_state(fd, PROG_AND_DATA);
    Check_Error(err);
    if (close(fd) < 0)
    {
	Set_Errno;
	Bip_Error(SYS_ERROR);
    }
    Succeed_;

#endif /* AS_EMU */
}
/* ***************************************************************** */
int
p_save_program(value v, type t)
{
    int		err;
    char *	name;

    Get_Name(v,t,name);

    err = save_program(name);
    Check_Error(err);
    Succeed_;
}
#else
int p_save(void)		/* it is referenced from emu.c */
{Bip_Error(NOT_AVAILABLE);}
int p_save_program(void)
{Bip_Error(NOT_AVAILABLE);}
#endif
/* ***************************************************************** */
int
save_program(char *name)
{
    int fd;
    int err;

    /* check if no saving from another level than level 1	     */
    if (g_emu_.nesting_level > 1)
    {
	Bip_Error(WRONG_LEVEL);
    }
    err = _create_saved_file(name, &fd);
    Check_Error(err);

    err = _save_state(fd, PROG_ONLY);

    Check_Error(err);
    if (close(fd) < 0)
    {
	Set_Errno
	Bip_Error(SYS_ERROR)
    }
    Succeed_;
}

/* ***************************************************************** */
/*
 * restore the state and return the saved state type (>= 0)
 * or an error code (< 0)
 * CAUTION: this function may be called from main() before the
 * Prolog system is initialised. Do not rely on anything!
 */

int
restore(char *name)
{
    ss_header 	header;
    int		fd;
    int		err;
    char	buf[MAX_PATH_LEN];

    name = expand_filename(name, buf);
    if ((fd = ec_open(name, 0)) < 0)
    {
	return SYS_ERROR;
    }

    err = read(fd, (char *) &header, sizeof(ss_header));
    if (err != sizeof(ss_header))
    {
	(void) close(fd);
	if (err == -1) return SYS_ERROR;
	return UNEXPECTED_EOF;
    }

    /* check the version and the start address		     */
    if (   header.magic != SAVED_STATE_MAGIC
	|| header.version != SAVED_STATE_VERSION
	||
	    /*** this condition could be relaxed as follows
	    header.sstype == PROG_AND_DATA &&
	    ***/
	    (header.engine != (char *) &g_emu_
	    || header.s_start != (char *) start_of_stacks)
	)
    {
	(void) close(fd);
	return INVALID_SS;
    }

    if (shared_mem_restore(&global_heap, fd) < 0)
    {
	Set_Errno;
	(void) close(fd);
	return SYS_ERROR;
    }

    err = _restore_stacks(fd, &header);
    if (err != PSUCCEED)
    {
	(void) close(fd);
	return err;
    }

    if (close(fd) < 0)
    {
	return SYS_ERROR;
    }
    return header.sstype;
}

/* *****************************************************************	*/
/*
 * restore the state from the specified file
 */
#if defined(HAVE_MMAP)
int
p_restore(value v, type t)
{
    char *	name;
    char	safe_name[MAX_PATH_LEN];
    int		sstype;

    Get_Name(v,t,name);

    (void) strcpy(safe_name, name);

    flush_and_close_io(0);

    sstype = restore(safe_name);
    if (sstype < 0)
    {
	if (sstype == SYS_ERROR)
	{
	    Set_Errno
	}
	Bip_Error(sstype)
    }
    if (sstype == PROG_ONLY)	
	longjmp(reset, 2);
    else if (sstype == REALLOCABLE)
	longjmp(reset, 3);
    else if (sstype == PROG_AND_DATA)
	longjmp(reset, 4);
    Bip_Error(ILLEGAL_RETURN)
}
#else
int p_restore(void)
{Bip_Error(NOT_AVAILABLE);}
#endif
