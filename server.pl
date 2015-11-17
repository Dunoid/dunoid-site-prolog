:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).

% Custom template module
:- use_module(templates).

%File locations
:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(files, '/', []).

:- http_handler(files(.), safe_file_reply, [prefix]). 

%Automatically handle invalid directories with a 404
safe_file_reply(Request) :-
	http_reply_from_files('assets/', [], Request);
	error_page(404, 'File Not Found', page_missing).

%Pages

:- http_handler(root(.), home_page, []).
:- http_handler(root(art), paginated('Art'), []).

/* TODO: Implement these pages
:- http_handler(root(programming), paginated('Programming'), []).
:- http_handler(root(videos), paginated('Videos'), []).
:- http_handler(root(mail), mailing_form, []).
:- http_handler(root(admin), admin_login, []).
:- http_handler(root(db), database, []).
*/

%Starting the server
html_set_options( [dialect(xhtml)] ).
:- http_server(http_dispatch, [port(8000)]).


home_page(_) :-
	basic_page(
		'Home',
		html([
			h2('About Me'),
			p(['My name is Devin Hastings, but I generally go by Dunoid when online, hence the ', 
				'site\'s domain name.  I\'m an undergraduate majoring in Computer Science with ',
				'minors in Mathematics and -if I can work in some summer credits- Russian.']),
			p(['You can see my recent programming projects ', a(href=programming, 'here.')]),
			p(['Besides that, I fancy myself an ameteur artist.  I draw, compose, and sometimes ',
				'even write.']),
			p(['To see my art, ', a(href=art, 'click here.')])
		])).


/*-- This was for testing the database functionality.
     it will likely be reworked for a mailing list

form_handler(Request) :-
	catch(
		form_page(Request),
		_E, error_page(500,'Submission failed!', _E)
	).
	
form_page(Request) :-	
	catch(
		http_parameters(Request, [ name(Name,[atom]), age(Age,[between(3, 150)]) ] ),
		_E, throw(_E)
	),
	add_person(Name, Age),
	basic_page(
		'Submission successful',
		html([
			h2('Submission successful'),
			p('Name: ~w'-Name),
			p('Age: ~w'-Age)
		])
	).

search_page(_) :-
	basic_page(
		'Search',
		html([ 
			div(class=sub,[
				h2('Search People'),
				p('Use an asterisk (*) for a wildcard.'),
				%Weird, evil things are happening with this form
				\name_age_form(search, search_people, get, 2, 'search_people')
			]),
			div([class=sub,id=search_results],'')
		])
	).

search_people(Request) :-
	catch(
		http_parameters(Request, [ name(Name,[atom]), age(Age,[atom]) ] ),
		_E,  fail
	),
	results_as_text(Name, Age, Text),
	text_page(Text);
	%If the search fails, it defaults to this result
	text_page('There was an error in your search.').
*/