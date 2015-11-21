:- module(templates,[ 
                    basic_page/2,
                    error_page/3,
                    paginated/2,
                    paginated/3,
                    link_button//3,
					login_form//2,
					link_button//3
                    ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).

:- use_module(pages).
:- use_module(server_io).

/* 	Page Content 
*	elements that appear within a page.
*	these aren't meant for use outside the module.
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
		[href=URL, style='color:black'],
		div([class=navigation, style=Style], Text)
	)).
	
login_form(Action, Id) -->
	html(
		form([
			name=login, 
			action=Action,
			method=post,
			autocomplete=off
		],[
			'Username:',br(/),
			input([type=text, name=uid, class=navigation]),
			br(/),br(/),
			'Password:',br(/),
			input([type=password, name=p, class=navigation]),
			br(/),br(/),
			input([type=submit, value='Login'])
		])
	).


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
		http_parameters( Request, [ page(Page, [number, default(1)]) ] ), _E, 
		error_page(500, 'Page Request Error', _E)
	),
	downcase_atom(Name, Mode),
	format(atom(Title), '~w - Page ~w', [Name, Page]),
	
	End is Page*Length,	%Getting the indeces of the files
	Start is End-(Length-1),
	
	%Making our query link
	atom_concat(Mode, '?page=', Query),
	
	N is End+1,     %The ID for the next page to check if it exists
	(get_file(N, Mode, _) ->
		(Next is Page+1, atom_concat(Query, Next, NextLink) );	
		NextLink='javascript:void(0)'),
	
	(Page > 1 -> 
		(Prev is Page-1, atom_concat(Query, Prev, PrevLink) );	
		PrevLink='javascript:void(0)'),
	
	basic_page(
		Title,
		html([
			\blerb(Page, Mode),     %blerb contains title and page description
			\get_content(Start, End, Mode),
			div(id=navbox, [
				\link_button(PrevLink, 'float:left', '<<'),
				\link_button(NextLink, 'float:right', '>>')
			])
		])
	).

%Getting the content from multiple files

get_content(Index, End, Mode) -->
	{	%If these statements are false, it goes to the last line
		Index =< End,
		get_file(Index, Mode, File),  %Check if the file exists
		format(Filename, '~w/~w', [Mode, File]),
		file_string(Filename, String),
		Next is Index+1
	},
	html([
		\content_format(String, Mode),
		\get_content(Next, End, Mode) %Go to the next file
	]);
	html('').
