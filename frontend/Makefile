all: 
	main.exe; dune exec ./main.exe test.yay

nick: main.exe;
	dune exec ./main.exe test.yay

main.exe:
	dune build main.exe

tests:
	powershell -File ./lexParserTests/tests.ps1 ./main.exe

clean:
		dune clean

.PHONY: all main.exe tests clean