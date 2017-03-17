:- include('draw.pro').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ввод строки с консоли и преобразование ее в список токенов.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_expr(Tokens) :- read_loop([], Tokens).
read_loop(Acc, Tokens) :-
	peek_code(C), C == 10 -> reverse(Acc, Tokens), ! ;
	read_token(Tok), read_loop([Tok | Acc], Tokens).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Преобразование списка токенов в дерево.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

factor(un(Op, X)) --> unary_op(Op), factor(X).
factor(num(X)) --> [X], {number(X)}.
factor(id(X)) --> [X], {atom(X)}.
factor(E) --> [punct('(')], expr(E), [punct(')')].
power(bin(Op, X, Y)) --> factor(X), power_op(Op), expr(Y).
power(E) --> factor(E).
term(bin(Op, X, Y)) --> power(X), mul_op(Op), expr(Y).
term(E) --> power(E).
expr(bin(Op, X, Y)) --> term(X), add_op(Op), expr(Y).
expr(E) --> term(E).

unary_op('-') --> ['-'].
unary_op('ln ') --> ['ln '].
power_op('^') --> ['^'].
mul_op('*') --> ['*'].
mul_op('/') --> ['/'].
add_op('+') --> ['+'].
add_op('-') --> ['-'].

parse(Tree) :- read_expr(List), expr(Tree, List, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Упрощение дерева выражения.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify(Tree, Tree) :- one_step(Tree, NewTree), Tree == NewTree, !.
simplify(Tree, Result) :- one_step(Tree, NewTree), simplify(NewTree, Result).
	
one_step(id(X), id(X)) :- !.
one_step(num(X), num(X)) :- !.
one_step(un(Op, X), R) :- simplify(X, X1), un_step(Op, X1, R), !.
one_step(bin(Op, X, Y), R) :- simplify(X, X1), simplify(Y, Y1), bin_step(Op, X1, Y1, R), !.

%% Правила упрощения для унарного минуса.

un_step(-, num(0), num(0)) :- !.
un_step(-, un(-, X), X) :- !.

un_step(Op, X, un(Op, X)) :- !.

%% Правила упрощения для сложения.

bin_step(+, num(X), num(Y), num(Z)) :- Z is X + Y, !.
bin_step(+, X, num(0), X) :- !.
bin_step(+, num(0), Y, Y) :- !.

%% Правила упрощения для вычитания.

bin_step(-, num(X), num(Y), num(Z)) :- Z is X - Y, !.
bin_step(-, X, num(0), X) :- !.
bin_step(/, X, X, num(1)) :- !.
bin_step(*, X, num(0), num(0)) :- !.
bin_step(*, num(0), X, num(0)) :- !.
bin_step(-, num(0), Y, un(-, Y)) :- !.

%% Правила упрощения для умножения.

bin_step(*, num(X), num(Y), num(Z)) :- Z is X * Y, !.
bin_step(*, X, num(1), X) :- !.
bin_step(*, num(1), Y, Y) :- !.

%% Правила упрощения для деления.

bin_step(/, num(X), num(Y), num(Z)) :- Z is X / Y, !.
bin_step(/, X, num(1), X) :- !.

%% Правила упрощения для степени.

bin_step(^, num(X), num(Y), num(Z)) :- Z is X ** Y, !.
bin_step(^, _, num(0), num(1)) :- !.
bin_step(^, X, num(1), X) :- !.

%% Прочие случаи.

bin_step(Op, X, Y, bin(Op, X, Y)) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Преобразование дерева в строку результата.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

priority(+, 1).
priority(-, 1).
priority(*, 2).
priority(/, 2).
priority(^, 3).
priority(ln, 4).


show_num(X) :-
	Y is float(X), Z is float_integer_part(Y),
	Z =:= Y -> (T is truncate(Y), write(T)) ; write(X).

show_op1(num(X)) :- show_num(X).
show_op1(id(X)) :- write(X).
show_op1(X) :- write('('), show(X), write(')').

show_op2(num(X), _) :- show_num(X).
show_op2(id(X), _) :- write(X).
show_op2(bin(Op, X, Y), N) :- priority(Op, M), M > N -> show(bin(Op, X, Y)).
show_op2(X, _) :- write('('), show(X), write(')').

show(num(X)) :- show_num(X).
show(id(X)) :- write(X).
show(un(Op, X)) :- write(Op), show_op1(X).
show(bin(Op, X, Y)) :- priority(Op, N), show_op2(X, N), write(Op), show_op2(Y, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Запуск программы.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% diff(un('-', X), un('-', A)) :- diff(X, A).


diff(num(_), num(0)).

diff(id(_), num(1)).

diff(bin('+', X, Y), bin('+', A, B)) :- diff(X, A), diff(Y, B).
diff(bin('-', X, Y), bin('-', A, B)) :- diff(X, A), diff(Y, B).

diff(bin('^', X, num(N)), bin('*', num(N), bin('^', X, bin('-', num(N), num(1))))).

diff(bin('*', X, Y), bin('+', bin('*', A, Y), bin('*', B, X))) :- diff(X, A), diff(Y, B).

diff(bin('/', X, Y), bin('/',  bin('-', bin('*', A, Y), bin('*', X, B)), bin('^', Y, num(2)))) :- diff(X, A), diff(Y, B).

diff(bin('^', F, G), bin('*', bin('^', num(2.71), bin('*', G, un('ln ', F))), bin('+', bin('*', B, un('ln ', F)), bin('/', bin('*', G, A), F)))) :- diff(F, A), diff(G, B).


:- initialization(start).
start :- (parse(Tree) ->
  (draw(Tree), diff(Tree, Diff), simplify(Diff, Result), draw(Result), show(Result))
  ; write('Syntax error!')), !.
