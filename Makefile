##
## EPITECH PROJECT, 2019
## Project libmy_malloc.so
## File description:
## Build libmy_malloc.so binary.
##

NAME	=	deBruijn

# CC	=	gcc -fpic

all:	$(NAME)

$(NAME):
	stack build --copy-bins --local-bin-path .
	mv project-exe $(NAME)


clean:
	stack clean
	rm -rf .stack-workdebruijn.cabal

fclean: clean
	rm -f $(NAME)

re: fclean all
