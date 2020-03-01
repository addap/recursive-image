# https://www.fpcomplete.com/blog/2015/05/haskell-web-server-in-5mb
default: image

build:
	@stack build
	@stack install --local-bin-path ./dist
#	@strip ./dist/initiative-counter

image: | build
	@docker build -t registry.gitlab.com/addapp/recursive-image/recursive-image:latest .

run: | image
	@docker run --rm -p 8023:8023 --name recursive-image -i -t registry.gitlab.com/addapp/recursive-image/recursive-image:latest

clean:
	@rm -rf hello

.PHONY: default build image run clean
