function renderMarkdowns()
{
    const md = markdownit({html:true})
          .use(texmath, { engine: katex,
                          delimiters: 'dollars'} );
    const elements = document.querySelectorAll('.markdown,.md');
    for (let elem of elements) {
	elem.innerHTML = md.render(elem.textContent);
    }
}

function init()
{
    renderMarkdowns();
}
