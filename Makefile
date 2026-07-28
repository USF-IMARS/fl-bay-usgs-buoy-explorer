.PHONY: render publish clean

render:
	quarto render

publish: render
	quarto publish gh-pages --no-prompt

clean:
	rm -rf _site .quarto
