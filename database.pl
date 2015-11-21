:- module(	email_db,
		 [	add_email/2,        %Name, Email
			remove_email/2,     %Name, Email
			search/2,           %Name, Email
			search_all/3,       %Name, Email, List
			results_as_text/3   %Name, Email, Text
		 ]).

:- use_module(library(regex)).
:- use_module(library(persistency)).

/*	This is a simple database for names and emails
*	for now, it has no real purpose, but I hope to
*	use it for a mailing list.
*/

:- persistent email(name:atom, email:atom).

% The database file
:- db_attach('email.pldb', []).

%Basic Operations
add_email(Name, Email) :-
	search(Name, Email);	%Ignore request if the email is already present
	assert_email(Name, Email).

remove_email(Name, Email) :-
	retractall_email(Name, Email).

search(Name, Email) :-
	email(Name, Email).

search_all(Name, Email, List) :-
	swap_wildcards(Name, FilterN),
	swap_number(Email, FilterE), !,
	findall([FilterN, FilterE], search(FilterN, FilterE), List).

%Dealing with wildcards in searches
swap_wildcards('*', _).
swap_wildcards(Other, Other).

swap_number('*', _). %If the wildcard is a number
swap_number(X, Y) :-
	atom_number(X, Y).


% Writing the results of a search as HTML text
results_as_text(Name, Email, Output) :-
	search_all(Name, Email, List), !,
	% Check if there were any results before writing.
	(List == [] -> Output = 'Your search returned no results.';
	result_text(List, Output)).

result_text([], 'No more results.').

result_text( [Head | Tail], L) :-
	result_text(Head, S),
	result_text(Tail, T), !,
	atom_concat(S, T, L).

result_text([Name, Email], S) :-
	format(atom(S),'~w: ~w <br />~n',[Name, Email]).
