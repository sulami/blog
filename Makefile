deploy:
	# Temporarily store uncommited changes
	git stash

	# Verify correct branch
	git checkout develop

	# Build new files
	stack exec blog clean
	stack exec blog build

	# Get previous files
	git fetch --all
	git checkout -B master --track origin/master

	# Overwrite existing files with new files
	cp -a _site/. .

	# Commit
	git add -A
	git commit -m "Publish."

	# Push
	git push origin master:master

	# Restoration
	git checkout develop
	git branch -D master
	git stash pop

live:
	stack exec blog clean
	stack exec blog watch
