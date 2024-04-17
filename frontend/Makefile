
all: 
		main.exe; dune exec ./main.exe test.yay

main.exe:
		dune build main.exe

tests:
		 powershell -File ./lexParserTests/tests.ps1 ./main.exe

testsNS:
		for f in tests/*.yay; do dune exec ./main.exe $$f; done

clean:
		dune clean

.PHONY: all main.exe tests clean