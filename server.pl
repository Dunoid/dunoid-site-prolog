:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).

% Custom template module
:- use_module(templates).
:- use_module(server_io).

%File locations
:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(files, '/', []).

:- http_handler(files(.), safe_file_reply, [prefix]). 

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

%ONE OF THESE MUST BE UN-COMMENTED FOR THE SERVER TO WORK
%For testing, port 8000 is used
%:- http_server(http_dispatch, [port(8000)]). 

%For public-facing servers, port 80 is used
%:- http_server(http_dispatch, [port(80)]).

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
