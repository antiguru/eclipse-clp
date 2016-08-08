% ----------------------------------------------------------------------
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
% The Original Code is  The eclipse_6 library for ECLiPSe.
% The Initial Developer of the Original Code is  Coninfer Ltd.
% Portions created by the Initial Developer are
% Copyright (C) 2016.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------

:- module(eclipse_6).

:- comment(categories, ["Compatibility"]).
:- comment(summary, "Compatibility definitions for ECLiPSe 6.X").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Joachim Schimpf, Coninfer Ltd").
:- comment(date, "$Date: 2016/08/08 14:34:24 $").

%----------------------------------------------------------------------

% the default was changed in ECLiPSe 7.0
:- export syntax_option(not based_bignums).

% Returns variable list in reverse order
:- export term_variables/2.
term_variables(T, Vs) :-
	sepia_kernel:term_variables_reverse(T, Vs).


% Obsolete alias for stream_select/3
:- export select/3.
select(Streams, Timeout, Ready) :-
	stream_select(Streams, Timeout, Ready).


%----------------------------------------------------------------------
end_of_file.
%----------------------------------------------------------------------

%
% subscript(+Matrix, +IndexList, ?Element)
% Differs from later version in that it returns nested lists instead
% of nested arrays for dim(M,[4,4]), subscript(M, [2..3,2..3], R)

:- export subscript/3.
:- tool(subscript/3, subscript/4).

subscript(Mat, Index, X, M) :-
	var(Index), !,
	( get_flag(coroutine,on) ->
	    suspend(subscript(Mat, Index, X, M), 2, Index->inst)
	;
	    error(4, subscript(Mat,Index,X), M)
	).
subscript(Mat, [], X, _M) :- !, X = Mat.
subscript(Mat, [IExpr|IExprs], X, M) :- !,
	subscript3(Mat, IExpr, X, M, IExprs).
subscript(Mat, Index, X, M) :-
	error(5, subscript(Mat,Index,X), M).

    subscript3(Mat, IExpr, X, M, IExprs) :-
	var(Mat), !,
	( get_flag(coroutine,on) ->
	    suspend(subscript(Mat,[IExpr|IExprs],X,M), 2, Mat->inst)
	;
	    error(4, subscript(Mat,[IExpr|IExprs],X), M)
	).
    subscript3(Mat, IExpr, X, M, IExprs) :-
	compound(Mat), !,
	subscript1(Mat, IExpr, X, M, IExprs).
    subscript3(Mat, IExpr, X, M, IExprs) :-
	is_handle(Mat), !,
	( IExprs = [] ->
	    eval(IExpr, I)@M,
	    xget(Mat, I, X)
	;
	    error(6, subscript(Mat,[IExpr|IExprs],X), M)
	).
    subscript3(Mat, IExpr, X, M, IExprs) :-
	string(Mat), !,
	( IExprs = [] ->
	    eval(IExpr, I)@M,
	    string_code(Mat, I, X)
	;
	    error(6, subscript(Mat,[IExpr|IExprs],X), M)
	).
    subscript3(Mat, IExpr, X, M, IExprs) :-
	error(5, subscript(Mat,[IExpr|IExprs],X), M).

    subscript1(Mat, IExpr, X, M, IExprs) :- integer(IExpr), !,
	arg(IExpr, Mat, Row),
	subscript(Row, IExprs, X, M).
    subscript1(Mat, Min..Max, Xs, M, IExprs) :- -?-> !,
	eval(Min, Imin)@M,
	eval(Max, Imax)@M,
	subscript2(Imin, Imax, Mat, IExprs, Xs, M).
    subscript1(Mat, IExpr, X, M, IExprs) :-
	eval(IExpr, I)@M,
	arg(I, Mat, Row),
	subscript(Row, IExprs, X, M).

    subscript2(Imin, Imax, Mat, IExprs, Xs, M) :-
	( Imin =< Imax ->
	    Xs = [X|Xs0],
	    +(Imin, 1, Imin1),
	    arg(Imin, Mat, Row),
	    subscript(Row, IExprs, X, M),
	    subscript2(Imin1, Imax, Mat, IExprs, Xs0, M)
	;
	    Xs = []
	).

