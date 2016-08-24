/*	Pages Module
*	I figured since paginated content was going to be big
*	on my site, I should dedicate a module to page formats.
*/

:- module(pages,[
                blerb//2,            %Page, Mode (initial content at the start of every page)
                content_format//2    %String, Mode (each piece of content)
				]).

% A small piece of text at the beginning of each page
blerb(Page, art) -->
	{Page == 1},  %Only show the big blerb on page 1
	html([
		h2(class=banner,'My Art'),
		p(class=[banner,blerb],[
			'More on ',
			a(href='http://dunoid.deviantart.com', 'DevantART')
		])
	]);
	html(h2(class=banner,'My Art')).

blerb(Page, programming) -->
	{Page == 1},
	html([
		h2(class=banner, 'My Projects'),
		p(class=[banner, blerb], [
			'Code available on ',
			a(href='https://github.com/Dunoid', 'Github')
		])
	]);
	html(h2(class=banner, 'My Projects')).

%This allows the same system to be reused on completely different types
% of content
content_format(String, test) -->
	html(p(String)).

content_format(String, art) -->
	{
	get_format(String, [Image, Desc])
	},
	html(
		div(class=banner, [
		img([class=art,src=Image]),
			br(/),
			p([class=[banner,blerb]],Desc)
		])
	);
	html('OOPS! This page had an error!').

content_format(String, programming) -->
	{ 
	get_format(String, [Title,Languages,Desc,Link | Image]),
	(Image == [] -> 
		(Class = 'programming-no-image', Width=75);
		(Class = [left, programming], Width = 100)
	)
	}, %Image is optional
	html([
		div(style='margin:0 auto;overflow:hidden;width=~w%;border-bottom:1px solid black;margin-bottom:10px;padding-bottom:15px;margin-bottom:20px;'-Width,[
			div( class=Class, [
				h2(Title),
				p(['Languages/Technologies Used:', br(/), b(Languages)]),
				hr(style='width:60%; float:left;'),
				br(/),
				a(href=Link, 'On Github'),
				br(/),
				p(Desc)
			]),
			\prog_image_div(Image)
		])
	]);
	html('OOPS!  This page had an error!').

prog_image_div(Image) -->
	{ \+ Image == [], [Link] = Image },
	html(
		div(class=[programming, left, 'p-img'], img([style='width:100%;',src=Link]))
	);
	html('').

get_format(String, List) :-
	atomic_list_concat(List, '#', String).
