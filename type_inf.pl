:- use_module(answers).

% s-expressions
clj_plain_sexpression(Atom) --> clj_atom(Atom).
clj_sexpression(Atom) --> clj_plain_sexpression(Atom).
clj_sexpression(Sequence) --> ['('], sequence_sexpressions(Sequence), [')'].
clj_sexpression(tuple(Sequence)) --> ['['], sequence_sexpressions(Sequence), [']'].
clj_sexpression(scope(Sequence)) --> ['{'], sequence_even_sexpressions(Sequence), ['}'].

% ������������������ s-���������
sequence_sexpressions([H|Tail]) --> clj_sexpression(H), !, sequence_delimiters_empty, sequence_sexpressions(Tail).
sequence_sexpressions([]) --> [].

% ������������������ � ������ ������ ���������
sequence_even_sexpressions([scope_record(F,S)|Tail]) --> clj_sexpression(F), sequence_delimiters,
                                                         clj_sexpression(S), sequence_delimiters_empty,
                                                         sequence_even_sexpressions(Tail).
sequence_even_sexpressions([]) --> [].

% ������� �������� ���� ��������� {�}_()
parser([]) --> [].
parser(Program) --> assign(Program).
assign(program(Scope, Exp)) --> sequence_delimiters_empty,
                                clj_sexpression(Scope), sequence_delimiters,
                                clj_sexpression(Exp), sequence_delimiters_empty.



% atoms: { �����, ������, �������������, �������� ����� }
clj_atom(loc_int(N)) --> clj_integer(N).
clj_atom(loc_string(Str)) --> ['"'], clj_string(Str), ['"'].
clj_atom(loc_identifier(Ident)) --> clj_identifier(Ident).
clj_atom(loc_keyword(Keyword)) --> clj_keyword(Keyword).

% keyword: {:identifier}
clj_keyword(X) --> [:], clj_identifier(Y), {string_concat(":", Y, String), atom_string(X, String)}.

% integer: { ������������������ ���� }
clj_integer(I) --> loc_digit(D0), loc_digits(D), {number_codes(I, [D0|D])}.

loc_digits([D|T]) --> loc_digit(D), !, loc_digits(T).
loc_digits([]) --> [].
loc_digit(D) --> [D], {code_type(D, digit)}.

% string: { ����� ������������������ ��������\{"} � ������� �������� }
clj_string(Str) --> quotes_escs(Chars),
                    {string_chars(Str, Chars)}.
quotes_escs([D|T]) --> quotes_esc(D), !, quotes_escs(T).
quotes_escs([]) --> [].
quotes_esc(D) --> [D], {D \== '"'}.                   

% identifier: {  ������������������ ����, ���� � ������������, ������������ � ����� | 
%                ������������������ ���� � ������������ | 
%                ������������������ ������������ }
clj_identifier(Ident) --> loc_letter(F), sequence_letter_digit_specials(Tail), {string_chars(Ident, [F|Tail])}.
clj_identifier(Ident) --> loc_special(F), sequence_digit_specials(Tail), {string_chars(Ident, [F|Tail])}.

% helper sequences
sequence_letter_digit_specials([D|T]) --> sequence_letter_digit_special(D), !, sequence_letter_digit_specials(T).
sequence_letter_digit_specials([]) --> [].
sequence_letter_digit_special(D) --> loc_letter(D) ; loc_special(D) ; loc_digit(D).

sequence_digit_specials([D|T]) --> sequence_digit_special(D), !, sequence_digit_specials(T).
sequence_digit_specials([]) --> [].
sequence_digit_special(D) --> loc_digit(D) ; loc_special(D).


sequence_delimiters --> loc_delimiter, sequence_delimiters_empty.
sequence_delimiters_empty --> [].
sequence_delimiters_empty --> loc_delimiter, sequence_delimiters_empty.

loc_letter(D) --> [D], {code_type(D, alpha)}.
loc_special(D) --> [D], {D == '+' ; D == '-' ; D == '>' ; D == '>' ; D == '=' ; D == '*'}.
loc_delimiter --> [' '] ; ['\t'] ; ['\n'] ; [','].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ���������� ������� ����� %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rec :-  open("tests.txt", read, In),
        open("result.txt", write, Out),
        process(In, Out, 1), 
        close(In), close(Out).

% �������� ������: ������ �� ����� �� ������� ';', ������������
process(In, Out, TestIdx) :- 
        \+ at_end_of_stream(In),!,
        read_string(In, ";", ";", _, Phrase),
        string_chars(Phrase, Chars),
        phrase(parser(X), Chars),
        X = program(scope(Scope), Exp),
        declare_scope(Scope, [], Context), !,
        write(Out, "Test #"), writeln(Out, TestIdx),
        write(Out, "my answer: "),
        (
                (
                        (
                        hindley_milner(Context, _, Exp, Answer, []),
                        nice_type(Answer, Out)
                        )
                ;
                        (
                        Answer = error,
                        write(Out, "error")
                        )
                )
        ),
        % ������� ���������� ����� �� ������ answers
        test_answer(TestIdx, TrueAnswer),
        write(Out, ", true answer: "), nice_type(TrueAnswer, Out),!,
        % ���������� ����������
        write(Out, ", eval: "), ((TrueAnswer == Answer, writeln(Out, "+"),!) ; (writeln(Out, "-"))),!,
        NextTestIdx is TestIdx + 1, 
        process(In, Out, NextTestIdx),!.
process(_, _, _).


% ����� ��� ������ ��������� � +- ������������ ����: � �������� ����� ��� ������
typestat_nice([], _).
typestat_nice([H|Tail], Out) :- H = var_type(V,T), write(V), write(" : "), nice_type(T, Out), put('\n'), typestat_nice(Tail, Out), !.

%nice_type(X) :- writeln(X), !.
nice_type(type(X,simple), Out) :- !, write(Out, X).
nice_type(type(pair(X,F),func), Out) :- !,write(Out, "(-> "), nice_type(X, Out), write(Out, " "), nice_type(F, Out), put(Out, ')').
nice_type(type(TypesList, tuple), Out) :- !, write(Out, "(cross"), nice_type_list(TypesList, Out), put(Out, ')').
nice_type(type(ListType, list), Out) :- !, write(Out, "(:List "), nice_type(ListType, Out), put(Out, ')').
nice_type(error, Out) :- !, write(Out, "error").
nice_type(undefined, Out) :- !, write(Out, "uncertain").

nice_type_list([H|Tail], Out) :- put(Out, ' '), nice_type(H, Out), nice_type_list(Tail, Out).
nice_type_list([], _).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ���� ��� �� � ������ {loc_keyword(Int) // loc_keyword(Str) // loc_keyword(Bool)} - ��� ������� ���: Int, Str, Bool
% ���� ��� � ������ � ������ ��������� = loc_identifier(->), �� ��� �������������� ���. �������� � ������������ �������� �������� ��� pair(Arg, Return).
% ���� ��� � ������ � ������ ��������� = loc_keyword(List), �� ��� ��� ������������������ List. ������ ������� � ������ - ��� ��������� � List.

% � ���� ����� ������� ���� �� ���������� ���������. ��������� � ������ � ������� var_type(Source, Type)
declare_scope([H|Tail], OldContext, ResContext) :-
                                                H = scope_record(loc_identifier(PureIdent),DirtyType),
                                                % ����������� ��� �� DirtyType
                                                create_linked_type(OldContext, NextContext, DirtyType, PureType),
                                                % ���������� ��������� ������� ���������
                                                declare_scope(Tail, NextContext, RestOfContext), 
                                                ResContext = [var_type(loc_identifier(PureIdent), PureType) | RestOfContext], !.
declare_scope([], _, []).


% ������� ���
create_linked_type(Context, Context, Source, Answer) :- 
                                        Source = loc_keyword(SimpleType), !,
                                        Answer = type(SimpleType, simple).

% ������� ���������� - ��� �������������: ���������, ��� ��� ���� � ���������
create_linked_type(Context, Context, Source, Answer) :-  
                                        Source = loc_identifier(_),
                                        member(var_type(Source, Answer), Context), !.

% ������� ���������� - ��������� �������: ���� �������� � ��������
create_linked_type(Context, [var_type(Source, Answer) | Context], Source, Answer) :- 
                                        Source = loc_identifier(_), 
                                        \+(member(var_type(Source, Answer), Context)), !. 

% �������������� ���: type(pair(Arg, Return), func)
create_linked_type(Context, New2Context, Source, Answer) :- 
                                        Source = [loc_identifier("->"), Lval, Rval], !,
                                        % ���������� ������� ��� ��������� � ������������� ��������
                                        create_linked_type(Context, New1Context, Lval, Ltype),
                                        create_linked_type(New1Context, New2Context, Rval, Rtype),
                                        Answer = type(pair(Ltype, Rtype), func).
% ��� ������: type(ListType, list)                               
create_linked_type(Context, NewContext, Source, Answer) :- 
                                        Source = [loc_keyword(':List'), DirtyListType], !,
                                        % ���������� ������� ��� ��������� ������
                                        create_linked_type(Context, NewContext, DirtyListType, ListType),
                                        Answer = type(ListType, list).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ����� ����� %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% [Const]
hindley_milner(OldContext, OldContext, Source, ResultType, _) :- 
                                        Source = loc_string(_), !,
                                        ResultType = type(':Str', simple).

hindley_milner(OldContext, OldContext, Source, ResultType, _) :-
                                        Source = loc_int(_), !,
                                        ResultType = type(':Int', simple).

% [Var with LetRec] - �������� �������� ��� LetRec. ���� ��������� ��������� X, ������� ���-�� ���� ������ � let-���������,
% ����� ������� ��� ���� ����� ����������, ������� � ���� ����� �������� ��� Y, ������������� ��.
% ����� ���� ��� Y ����� ��� ��� LetRec, �.�. ������ ��� ���������� ���������������� � �������� X
hindley_milner(OldContext, NewestContext, Source, ResultType, LetRec) :-  
                                        Source = loc_identifier(Ident), 
                                        LetRec = [letrec(loc_identifier(Ident), Y)], !,
                                        NewContext = [var_type(Source, W) | OldContext],
                                        hindley_milner(NewContext, NewestContext, Y, A, []),
                                        A = W, ResultType = W.

% [Var]
hindley_milner(OldContext, OldContext, Source, ResultType, _) :-  
                                        Source = loc_identifier(_),
                                        member(var_type(Source, ResultType), OldContext), !.

% [AVar2]
hindley_milner(OldContext, [var_type(Source, ResultType) | OldContext], Source, ResultType, _) :- 
                                        Source = loc_identifier(_),
                                        \+(member(var_type(Source, ResultType), OldContext)), !.

% [Tuple] - ������������ � ������� type(TypesList, tuple)
hindley_milner(OldContext, NewContext, Source, ResultType, LetRec) :- 
                                        Source = tuple(List),!,
                                        make_tuple_type(OldContext, NewContext, List, TypesList, LetRec),
                                        ResultType = type(TypesList, tuple).

make_tuple_type(OldContext, NewContext, [H|List], [A|Answer], LetRec) :- 
                                        hindley_milner(OldContext, SuccContext, H, A, LetRec),!,
                                        make_tuple_type(SuccContext, NewContext, List, Answer, LetRec).
make_tuple_type(OldContext, OldContext, [], [], _).

% [AApp]
hindley_milner(Context, ContextDerivedA, Source, ResultType, LetRec) :-   
                                        Source = [A, B], !,
                                        % ���������� ������� ���� ��� ������ � ����� �����
                                        hindley_milner(Context, ContextDerivedB, B, TypeB, LetRec),
                                        hindley_milner(ContextDerivedB, ContextDerivedA, A, TypeA, LetRec),
                                        % ����������� ����� ����� � (������ -> Result)
                                        TypeA = type(pair(TypeB, ResultType), func).

% [AAbs]
hindley_milner(OldContext, ContextAfterY, Source, ResultType, LetRec) :- 
                                        Source = [loc_identifier("lambda"), loc_identifier(Ident), Y], !,
                                        % ������ LetRec �� Ident, �.�. ����� ���������� ��������: ���������� let � lambda �������������
                                        delete(LetRec, letrec(loc_identifier(Ident), _), NiceLetRec),
                                        hindley_milner([var_type(loc_identifier(Ident), W) | OldContext], ContextAfterY, Y, TypeY, NiceLetRec),
                                        ResultType = type(pair(W, TypeY), func).

% [ALetRec]
hindley_milner(OldContext, NewContext, Source, ResultType, _) :-
                                        Source = [loc_identifier("let"), tuple([loc_identifier(X), Y]), Z], !,
                                        % � ����� ������� ����� ���-�� ��-���������� ��������� Z...
                                        % ����������� �����: ����� ��� ����������� ������� Z ��������� X, (���� �� �� ��� �������!!!)
                                        % ���� �������� ����� ���������� � ����� ����� w,
                                        % ����� ����� Y, ����� � ��� ����������� [x/x_i]
                                        % ������ ��� ����� ����� ��������� Y, ������ �������� ��������� ����������� � �������� x, �� ���� x_i
                                        % ������ ����� ���� ��� Y:a, ����� ���������� w = a
                                        hindley_milner(OldContext, NewContext, Z, ResultType, [letrec(loc_identifier(X), Y)]).