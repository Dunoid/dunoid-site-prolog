:- module(templates,[ %Exported statements
					basic_page/2,
					error_page/3,
					text_page/2,
					text_page/1,
					paginated/2,
					paginated/3,
					link_button//3
					]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).

/* 	Page Content 
*	elements that appear within a page.
*	these aren't generally used outside the module.
*/
header -->
	html(
		a( href(/), div( [class=banner,id=header], h1('Dunoid [WIP]') ) )
	).
footer -->
	html(
		div( [class=banner,id=footer], p(['Dunoid.com ',&(copy),' Devin Hastings']) )
	).

css(default) -->
	css('/files/style.css').

css(URL) -->
	html(link([ type('text/css'),
			rel('stylesheet'),
			href(URL)
		])).
		
js(default) -->
	js('/scripts/jquery-1.11.3.min.js'),
	js('/scripts/dunoid.js').

js(URL) -->
	html(script(
		[src(URL), type('text/javascript')], []
		)
	).
	
link_button(URL, Style, Text) -->
	html( a( 
		href(URL), input([type=button, style=Style, value=Text])
	)).

/* Generic name/age form for tests
name_age_form(Name, Action, Method, Id, OnSubmit) -->
	html(
		form([
			name(Name), 
			action(Action),
			method(Method),
			onsubmit('return ~w(this, ~w)'-[OnSubmit,Id]),
			autocomplete(off)
		],[
			'Name:',br(/),
			input([type(text), name(name)]),
			span([id('name_check~w'-Id), class(validate)], []),
			br(/),br(/),
			
			'Age:',br(/),
			input([type(text), name(age), id(age_input)]),
			span([id('age_check~w'-Id),class(validate)], []),
			br(/),br(/),
			
			input([type(submit), value('Submit')])
		])
	).
*/

/*	Page Templates
*	full pages using the above content
*/

basic_page(Title, Content) :-
	catch(
		reply_html_page(
			[title(Title), \css(default), \js(default)],
			[\header, div(id=content, Content),\footer]
		), 
		_E, error_page(500, 'Internal Server Error', _E) 
	).
	
error_page( ErrorType, Message, Error) :-
	format(atom(Title), 'Is Dead - ~w Error',ErrorType),
	basic_page(
		Title,
		html([
			div(class=sub, [
				h2('Error ~w - ~w'-[ErrorType,Message]),
				p('Error Type:\n~w'-Error),
				p('Maybe go back and try again?'),
				a(href(/), 'Home')
			] ),
			div(class=[sub, right], img([src('/files/500.svg'), width=200]))
		])
	).
	
% Basic text data for AJAX requests
% TODO: make method for sending JSON data

text_page(String, Data) :-
	format('Content-type:text/plain~n~n'),
	format(String, Data).
	
text_page(String) :-
	text_page(String, []).
	

/*	Paginated Template
*	this allows for loading an arbitrary number of files
*	onto a single page.
*
*	This isn't random access, which may be a problem in some uses.
*	It is also very specific in how it's used, which could be an 
*	issue in the long run.  Refactoring to allow a more flexible
*	system will likely be a future task (searching by file date?).
*/

paginated(Name, Request) :-   %Default page size is five files
	paginated(Name, 5, Request).

paginated(Name, Length, Request) :-
	catch(
		http_parameters( Request, [page(Page, [number, default(1)])] ), _E, 
		error_page(500, 'Page Request Error', _E)
	),
	downcase_atom(Name, Mode),
	format(atom(Title), '~w - Page ~w', [Name, Page]),
	
	End is Page*Length,	%Getting the indeces of the files
	Start is End-(Length-1),
	
	%Making our query link
	atom_concat(Mode, '?page=', Query),
	
	N is End+1,
	format(atom(Link), 'assets/~w/~w.txt', [Mode, N]), %The files for the next page
	(exists_file(Link) 
		->	(Next is Page+1, atom_concat(Query, Next, NextLink) )
		;	NextLink='javascript:void(0)'),
	
	(Page > 1 
		-> 	(Prev is Page-1, atom_concat(Query, Prev, PrevLink))
		;	PrevLink='javascript:void(0)'),
	
	basic_page(
		Title,
		html([
			\blerb(Page, Mode),
			\( templates:get_content(Start, End, Mode) ),
			div(id=navbox, [
				\link_button(PrevLink, 'float:left', '<<'),
				\link_button(NextLink, 'float:right', '>>')
			])
		])
	).
	
blerb(Page, art) --> %Only show the big blerb on page 1
	{Page == 1},
	html([
		h2(class=banner,'My Art'),
		p([class=banner,style='max-width:650px;'],[
			'I\'m no master artist, but I like to think I\'m',
			' alright at it.  And I hope to keep improving.'
		]),
		p([class=banner,style='max-width:650px;'],[
				'For more of my art, go to ', 
				a(href='http:/dunoid.deviantart.com', 'my DevantART account'),
				'.'
		])
	]);
	html(h2(class=banner,'My Art')).
	
%Getting the content from multiple files

get_content(Index, End, Mode) -->
	{
		Index =< End,	%If these statements are false, it goes to the last line
		format(atom(File), 'assets/~w/~w.txt', [Mode,Index]),
		exists_file(File),
		read_file(File, Desc),
		Next is Index+1
	},
	html([
		\content_format(Index, Desc, Mode),
		\get_content(Next, End, Mode) %Go to the next file
	]);
	html('').

%This allows the same system to be reused on completely different types of content
content_format(Index, Desc, art) -->
	html(
		div(class=banner, [
			img([style='max-width:85%; max-height:90%',src='/art/~w.png'-Index]),
			br(/),
			p([class=banner,style='height:100px; max-width:650px;'],Desc)
		])).

/*	Other Functions
*	misc utilities that I need for my site
*/

%read a generic text file to an atom
read_file(File, Out) :-
	file_string(File, Str),
	atom_string(Out, Str).

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
	