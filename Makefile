serve:
	cd _site && python3 -m http.server
watch:
	fd .org | entr emacs --batch -l config.el --file /_ --eval '(blog/render-file)'
