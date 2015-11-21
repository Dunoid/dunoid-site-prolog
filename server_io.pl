/*	Server_IO Module:
*	For low-level utilities, generally anything
*	that deals with reading files or plain text.
*/
:- module(server_io,[
                    text_page/2,    %Format, Data
                    text_page/1,    %String
                    file_string/2,  %File, Out
                    add_file/2,     %Mode, FileName
                    get_file/3,     %ID, Mode, FileName
					get_files/2,    %Mode, Pairs
					add_user/2,     %UID, Password
					check_user/3    %UID, Password, Role
                    ]).

:- use_module(library(persistency)).	
:- use_module(library(gensym)).

/*	
*	Databases
*/	

:- persistent file(id:integer, mode:atom, filename:atom). %For written entries
:- persistent user(uid:atom, role:oneof([author, user]), hash:atom, salt:float ). %For users

:- db_attach('data.pl', []).

%For the files database

add_file(Mode, Filename) :- % Maybe assume the data is in order and just grab the top ID?
	\+file(_, Mode, Filename), %Fail if there's already a file with this name
	max_id(Mode, ID), % Largest ID
	Next is ID+1,
	assert_file(Next, Mode, Filename);
	assert_file(1, Mode, Filename).

get_file(ID, Mode, FileName) :-
	file(ID, Mode, FileName).

get_files(Mode, Pairs) :-
	findall(ID-Filename, file(ID, Mode, Filename), Pairs).

max_id(Mode, Max) :-
	get_files(Mode, Pairs),
	\+(Pairs == []),
	pairs_keys(Pairs, Keys),
	max_list(Keys, Max).

%For the user database
add_user(UID, Password) :-
	\+user(UID, _, _, _), %Fail if the user already exists
	random(Salt),
	atom_concat(Password, Salt, Salted),
	variant_sha1(Salted, Hash),
	assert_user(UID, user, Hash, Salt).

check_user(UID, Password, Access) :- %Check user's password and access
	user(UID, _, _, Salt),
	catch(
		( atom_concat(Password, Salt, Salted),
		variant_sha1(Salted, Hash) ), _, fail
	),
	user(UID, Access, Hash, Salt).
	
	
/*	
*	Basic file and text functions
*/	
					
% Basic text data for AJAX requests
% TODO: make method for sending JSON data

text_page(String, Data) :-
	format('Content-type:text/plain~n~n'),
	format(String, Data).

text_page(String) :-
	text_page(String, []).

%Parse my specific file format
file_parse(File, StringList) :-
	file_string(File, S),
	split_string(S, '#', '~n', StringList).
	
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