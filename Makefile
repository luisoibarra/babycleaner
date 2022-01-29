ghci:
	make build
	stack ghci app/Main.hs

run:
	make build
	stack exec babycleaner-exe

clean:
	stack clean

build:
	stack build