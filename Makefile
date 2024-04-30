all: 
	numatix.exe; dune exec ./numatix.exe test.numa

nick: numatix.exe;
	dune exec ./numatix.exe frontend/test.numa

main.exe:
	dune build numatix.exe

tests:
	powershell -File ./frontend/lexParserTests/tests.ps1 ./numatix.exe

clean:
	dune clean

.PHONY: all numatix.exe tests clean
