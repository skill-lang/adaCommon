.PHONY:clean debug release main valgrind

main: debug

clean:
	gprclean gnat/test.gpr -Xmode=debug
	gprclean gnat/test.gpr -Xmode=release

debug:
	gprbuild gnat/test.gpr -Xmode=debug

release:
	gprbuild gnat/test.gpr -Xmode=release

valgrind:
	valgrind --dsymutil=yes --leak-check=full --show-leak-kinds=all ./release
