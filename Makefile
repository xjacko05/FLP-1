.PHONY: clean test zip all

all:
	ghc -Wall main.hs -o flp22-fun

test: all
	@./test.sh

clean:
	@rm ./flp22-fun ./*.o ./*.hi ./tests/*.in ./tests/*.out ./tests/*.real tests/key_self tests/sign_self

zip:
	zip -r flp-fun-xjacko05.zip main.hs Makefile test.sh README.md tests