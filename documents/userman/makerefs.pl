% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
%
% eclipse -e "[makerefs],do"
%
% preprocesses files to insert (or update) hyperlinks to bip pages:
%
%	\bip{n/a}
%	{\bf n/a}
%	\bipref{n/a}{oldurl}
%
%  -->	\bipref{n/a}{url}
%
% and
%	\biptxt{text}{n/a}
%     	\biptxtref{text}{n/a}{oldurl}
%
%  -->	\biptxtref{text}{n/a}{url}
%
% and
%       \txtbip{text}{n/a}
%       \txtbipref{text}{n/a}{oldurl}
%  -->  \txtbipref{text}{n/a}{url}
%
% txtbip* and biptxt* differs in what gets put into the index:
% In txtbip*, it is text that is put into the index; 
% In biptxt*, it is n/a that is put into the index.
% Both uses n/a to find the url.

:- ['../../doc/bips/index'].
:- set_chtab(0'`, list_quote).

do :-
	read_directory(., "*.tex", _, Files),
%	argv(all, [_|Files]),
	do(Files).

do([]).
do([F|Fs]) :-
	do(F),
	do(Fs).
do(File) :-
	string(File),
	printf("%s ... %b", [File]),
	open(File, read, S),
	read_string(S, "", _, String),
	close(S),
	string_list(String, List),

	get_flag(pid, Pid),
	concat_string(["tmp",Pid], TmpFile),
	open(TmpFile, write, out),
	( top(List, []) ->
	    concat_string([File,".bak"],BakFile),
	    rename(File,BakFile),
	    rename(TmpFile,File)
	;
	    printf("**** FAILED on file %s\n", [File])
	),
	writeln(done),
	close(out).


top -->
	`\\bip{`,
	latexarg(BipList),	% consumes closing }
	{
	    string_list(BipString, BipList),
	    emit_bipref(BipString)
	},
	!,
	top.
top -->
	`\\biptxt{`,
	latexarg(TextList),	% consumes closing }
	`{`,
	latexarg(BipList),	% consumes closing }
	{
	    string_list(BipString, BipList),
	    string_list(TextString, TextList),
	    emit_bipref(TextString, "biptxt", BipString)
	},
	!,
	top.
top -->
	`\\txtbip{`,
	latexarg(TextList),	% consumes closing }
	`{`,
	latexarg(BipList),	% consumes closing }
	{
	    string_list(BipString, BipList),
	    string_list(TextString, TextList),
	    emit_bipref(TextString, "txtbip", BipString)
	},
	!,
	top.
%top -->
%	`{\\bf `,
%	latexarg(BipList),	% consumes closing }
%	{
%	    string_list(BipString, BipList),
%	    emit_bipref(BipString)
%	},
%	!,
%	top.
top -->
	`\\bipref{`,
	latexarg(BipList),	% consumes closing }
	`{`,
	latexarg(_OldUrl),
	{
	    string_list(BipString, BipList),
	    emit_bipref(BipString)
	},
	!,
	top.
top -->
	`\\biptxtref{`,
	latexarg(TextList),	% consumes closing }
	`{`,
	latexarg(BipList),	% consumes closing }
	`{`,
	latexarg(_OldUrl),
	{
	    string_list(BipString, BipList),
	    string_list(TextString, TextList),
	    emit_bipref(TextString, "biptxt", BipString)
	},
	!,
	top.
top -->
	`\\txtbipref{`,
	latexarg(TextList),	% consumes closing }
	`{`,
	latexarg(BipList),	% consumes closing }
	`{`,
	latexarg(_OldUrl),
	{
	    string_list(BipString, BipList),
	    string_list(TextString, TextList),
	    emit_bipref(TextString, "txtbip", BipString)
	},
	!,
	top.
top -->
	[C],
	{ put(out, C) },
	top.
top --> [].

latexarg([]) --> `}`, !.
latexarg([C|Cs]) --> [C], latexarg(Cs).


emit_bipref(BipString) :-
	try_term_string(Bip, BipString),
	( Bip = (N/A) ; Bip = (SubGroup:N/A) ; Bip = (N/A,_) ),
	nonvar(N), nonvar(A),
	findbip(N, A, Group, SubGroup, File0),
	!,
	( File0=='' -> File = index ; File = File0 ),
	concat_string([Group,/,SubGroup,/,File,".html"], HtmlFile),
	printf(out, "\\bipref{%s}{../bips/%s}", [BipString,HtmlFile]).
emit_bipref(BipString) :-
	printf("*** Could not find %s%n", [BipString]),
	fail.

emit_bipref(Text, BipTxtType, BipString) :-
	try_term_string(Bip, BipString),
	( Bip = (N/A) ; Bip = (SubGroup:N/A) ; Bip = (N/A,_) ),
	nonvar(N), nonvar(A),
        findbip(N, A, Group, SubGroup, File0),
	!,
	( File0=='' -> File = index ; File = File0 ),
	concat_string([Group,/,SubGroup,/,File,".html"], HtmlFile),
	printf(out, "\\%sref{%s}{%s}{../bips/%s}", [BipTxtType,Text,BipString,HtmlFile]).
emit_bipref(_, _, BipString) :-
	printf("*** Could not find %s%n", [BipString]),
	fail.

findbip(N, A, Group, SubGroup, File) :-
        (var(SubGroup) -> UnkSubG = yes ; UnkSubG = no),
	findall(f(File0,Group0,SubGroup), bip(N, A, Group0, SubGroup, File0), 
                [f(File,Group,SubGroup)|Tail]),
        (Tail == [] -> true
        ;
         UnkSubG == no -> printf("*** More than one predicate match %w:%w/%w%n*** Group %w used.%n", [SubGroup,N,A,Group])
        ;
         UnkSubG == yes, printf("*** More than one predicate match %w/%w%n*** Group %w-%w used.%n", [N,A,Group,SubGroup])
        ).
        

try_term_string(T, S) :-
	set_error_handler(114, fail/0),
	set_error_handler(115, fail/0),
	set_error_handler(117, fail/0),
	set_error_handler(119, fail/0),
	set_error_handler(198, fail/0),
	set_error_handler(7, fail/0),
	current_op(Prec1,Assoc1,(',')),
	current_op(Prec2,Assoc2,(/)), !,
	current_op(Prec3,Assoc3,(:)), !,
	op(1200,yfx,(',')),
	op(1199,yfx,(:)),
	op(1198,yfx,(/)),
	set_flag(macro_expansion,off),
	( term_string(T, S) ->
%	    printf("Parsed: %q\n", [T]),
	    true
	;
	    printf("*** Couldn't parse: %q\n", [S]),
	    T=S
	),
	op(Prec1,Assoc1,(',')),
	op(Prec2,Assoc2,(/)),
	op(Prec3,Assoc3,(:)),
	reset_error_handler(7),
	reset_error_handler(114),
	reset_error_handler(115),
	reset_error_handler(117),
	reset_error_handler(198),
	reset_error_handler(119).


