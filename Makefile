serve:
	cd _site
	python -m SimpleHTTPServer
watch:
	fd .org | entr emacs --batch -l config.el --file /_ --eval '(blog/render-file)'
