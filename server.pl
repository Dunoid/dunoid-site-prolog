:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).

:- use_module(library(http/http_session)).

% Custom template module
:- use_module(templates).
:- use_module(server_io).
:- use_module(pages).

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
:- http_handler(root(programming), paginated('Programming'), []).

:- http_handler(root(login), login, []).
:- http_handler(root(add), add_entry, []).
:- http_handler(root('add/preview'), preview_page, []).
:- http_handler(root('add/write'), write_page, []).

/* TODO: Implement these pages
:- http_handler(root(videos), paginated('Videos'), []).
:- http_handler(root(mail), mailing_form, []).
:- http_handler(root(db), database, []).
*/

%Starting the server
html_set_options( [dialect(xhtml)] ).
start_server(Port):- http_server(http_dispatch, [port(Port)]). 

%ONE OF THESE MUST BE UN-COMMENTED FOR THE SERVER TO WORK
%For testing, port 8000 is recommended.  For public-facing servers, port 80

% ?- start_server(8000).
% ?- start_server(80).

home_page(_) :-
	basic_page(
		'Home',
		html([
			h2('About Me'),
			p(['My name is Devin Hastings, but I generally go by Dunoid when online, hence the ', 
				'site\'s domain name.  I\'m an undergraduate majoring in Computer Science with ',
				'minors in Mathematics and -if I can work in some summer credits- Russian.']),
			p(['In addition to my education, I often work on side projects to keep me busy.  ',
				 'You can see my recent programming projects ', a(href=programming, 'here.')]),
			p(['Besides that, I fancy myself an ameteur artist.  I draw, compose, and sometimes ',
				'even write.']),
			p(['To see my art, ', a(href=art, 'click here.')])
		])).

login(_) :-
	basic_page(
		'Login',
		html([
			h2('Login'),
			\login_form(add, 1)
		])).

add_entry(Request) :-
	catch(
		http_parameters( 
			Request, [
			uid(Username, [atom, default(default)]), 
			p(Password, [atom, default(default)])
		] ),
		_E, (error_page(500, 'Server Request Failed', _E), fail)
	),
	(
		check_user(Username, Password, author),
		http_session_assert(author),
		basic_page(
			'Write an Entry',
			html([
				div(class=left,[
					p(['Page Type:', br(/),
						\button('Programming'),
						\button('Art'),
						\button('Test')
					]),
					'Title', br(/),
					input([id=filename, class=input,value='File Name']), br(/),
					textarea([rows=8, cols=40, id=writebox, class=input],'TODO: Make templates for pages'),
					br(/),
					div(id=navbox, [
						\link_button('javascript:submit_page("preview");','float:left;','Preview'),
						\link_button('javascript:submit_page("write");', 'float:right;','Create')
					])
				]),
				div([class=left, id=results], "")
			])
		);
		error_page(403, 'Access Denied', 'incorrect username/password')
	);
	write('<!--OOPS-->').
	
button(Type) -->
	{downcase_atom(Type, Downcase)},
	html([
		input([type=radio,name=mode_radio, value=Downcase], Type)
	]).

preview_page(Request) :-
	catch(
		http_parameters( 
			Request, [
			data(DataAtom, [atom]),
			mode(Mode, [atom])]
		),
		_E, (text_page('Submission error:<br>~w',_E), fail)
	),
	atom_string(DataAtom, Data),
	phrase(content_format(Data, Mode), HTML, []),
	format('Content-type: text/html~n~n'),
	
	print_html(HTML).
	
write_page(Request) :-
	http_session_data(author),
	catch(
		http_parameters(
			Request, [
			mode(Mode, [atom]),
			file(FileAtom, [atom]),
			data(Content, [atom])
		]),
		_E, (text_page('There was an error:~n<br>~w',_E), fail)
	),
	format('Content-type:text/plain~n~n'),
	
	(\+ add_file(Mode, FileAtom) -> 
		(write('failed to add file\n'), fail)
		;true),
	split_string(FileAtom, " ", " ", L),
	atomics_to_string(L, "-", Filename), %replace spaces with dashes
	write('Writing file now...\n'),
	format(atom(Output), 'assets/~w/~w.txt', [Mode, Filename]),
	
	catch(
		(
		open(Output, write, Stream),
		write('Found the file\n'),
		write(Stream, Content),
		close(Stream)
		), 
		_E, 
		(	
		format('Error:~n~w',_E), 
		server_io:retractall_file(_,Mode,FileAtom), 
		fail
		)
	),
	write('The upload was successful\n');
	
	write('The upload failed.').
