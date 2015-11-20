/*	Pages Module
*	I figured since paginated content was going to be big
*	on my site, I should dedicate a module to page formats.
*/

:- module(pages,[ 
                blerb//2,            %Page, Mode (initial content at the start of every page)
                content_format//3    %Index, String, Mode (each piece of content)
				]).

blerb(Page, art) -->
	{Page == 1},  %Only show the big blerb on page 1
	html([
		h2(class=banner,'My Art'),
		p(class=[banner,blerb],[
			'I\'m no master artist, but I like to think I\'m',
			' alright at it.  And I hope to keep improving.',
			br(/),br(/),
			'For more of my art, go to ',
			a(href='http://dunoid.deviantart.com', 'my DevantART account'),
			'.'
		])
	]);
	html(h2(class=banner,'My Art')).

%This allows the same system to be reused on completely different types of content
content_format(Index, Desc, art) -->
	html(
		div(class=banner, [
			img([class=art,src='/art/~w.png'-Index]),
			br(/),
			p([class=[banner,blerb]],Desc)
		])).

