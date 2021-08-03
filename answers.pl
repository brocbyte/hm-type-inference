:- module(answers, [test_answer/2]).
% формат ответов соответствует внутреннему представлению в программе
test_answer(1, type(':Int', simple)).
test_answer(2, error).
test_answer(3, type(':Int', simple)).
test_answer(4, error).
test_answer(5, type([type(':Int',simple), type(':Int',simple)],tuple)).
test_answer(6, type(pair(type(':Int', simple), type(':Bool', simple)), func)).
test_answer(7, error).
test_answer(8, type(':Int', simple)).
test_answer(9, type(':Int', simple)).
test_answer(10, type(pair(type(':Int', simple), type(':Int', simple)), func)).
test_answer(11, type(':Int', simple)).
test_answer(12, type([
                type(pair(type(':Str', simple), type(':Str', simple)), func),
                type(pair(type(':Int', simple), type(':Int', simple)), func)],
                tuple)).
test_answer(13, type(':Int', simple)).
% в связи с наличием типовых переменных в полученных мною ответах в 14 и 16 тестах я не совсем уверен в их правильности,
% поэтому не заношу их в "ответы". undefined - заглушка, на выводе в графе "true answer" будет "uncertain"
test_answer(14, undefined).
test_answer(15, type(':Int', simple)).
test_answer(16, undefined).
% note: type for 'times' looks like: (Int -> ((k -> k) -> (type(x) -> k)))