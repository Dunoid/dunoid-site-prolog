
/*	Server_IO Module:
*	For low-level utilities, generally anything
*	that deals with reading files or plain text.
*/

:- module(server_io,[
                    text_page/1,    %String
					text_page/2,    %Format, Data
					file_string/2   %File, Out
					]).

% Basic text data for AJAX requests
% TODO: make method for sending JSON data

text_page(String, Data) :-
	format('Content-type:text/plain~n~n'),
	format(String, Data).

text_page(String) :-
	text_page(String, []).

%read a generic text file as a string
file_string(File, Out) :-
	open(File, read, Str),
	get_code(Str, Char),
	get_chars(Str, Char, CharList),
	string_codes(Out, CharList),
	close(Str).

get_chars(_, -1, []) :- !. %EOF character

get_chars(Stream, Char, [Char|More]) :-
	get_code(Stream, CharNext),
	get_chars(Stream, CharNext, More).
