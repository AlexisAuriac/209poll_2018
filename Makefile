##
## EPITECH PROJECT, 2018
## 209poll
## File description:
## Makefile for 209poll.
##

NAME	=	209poll

all:
	make $(NAME)

$(NAME):
	stack build
	cp `stack path --local-install-root`/bin/$(NAME) .

.PHONY:	all $(NAME)
