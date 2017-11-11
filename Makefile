deploy:
	# Temporarily store uncommited changes
	git stash

	# Verify correct branch
	git checkout develop

	# Update sources
	git push origin develop:develop

	# Build new files
	stack exec blog clean
	stack exec blog build

	# Get previous files
	git fetch --all
	rm -rf tufte
	git checkout -b master --track origin/master

	# Overwrite existing files with new files
	rsync -a 								   \
		--filter='P _site/'      \
		--filter='P _cache/'     \
		--filter='P .git/'       \
		--filter='P .gitignore'  \
		--filter='P .stack-work' \
		--delete-excluded        \
		_site/ .

	# Commit
	git add -A
	git commit -m "Publish."

	# Push
	git push origin master:master

	# Restoration
	git checkout develop
	git branch -D master
	git submodule update -f
	git stash pop || true

build: clean
	stack exec blog build

live: clean
	stack exec blog watch

clean:
	stack exec blog clean
