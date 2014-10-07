TEMPLATE = 'template.f08'
FC = gfortran

.DEFAULT: new

.PHONY: run test clean

__strip_zeros = $(shell echo "$(1)" | sed 's/^0*//')

__plus = $(shell echo $$(($(1) + $(2))))
plus = $(call __plus,$(call __strip_zeros,$1),$(call __strip_zeros,$2))

inc = $(call plus,$(1),1)

__minus = $(shell echo $$(($(1) - $(2))))
minus = $(call __minus,$(call __strip_zeros,$1),$(call __strip_zeros,$2))

dec = $(call minus,$(1),1)

exists = $(shell if [ -f $(1) ]; then echo "1"; else echo ""; fi)

problem_name = $(shell printf "src/euler%03d.f08" $(1))

problem_num = $(shell printf "%03d" $(1))

eq = $(shell if [ "$(1)" == "$(2)" ]; then echo "1"; else echo ""; fi)

ifdef f
no_force := $(shell if [ "$(f)" -eq "1" ]; then echo ""; else echo "1"; fi)
else
no_force := 1
endif

ifdef n
new_problem := $(n)
run_problem := $(n)
else
solutions := $(wildcard src/euler*.f08)
ifeq "$(solutions)" ""
new_problem := 1
run_problem := 1
else
new_problem := $(call inc,$(call __strip_zeros,$(subst euler,,$(basename $(notdir $(lastword $(sort $(solutions))))))))
run_problem := $(call __strip_zeros,$(subst euler,,$(basename $(notdir $(lastword $(sort $(solutions)))))))
endif
endif

new:
	$(if $(and \
		$(call exists,$(call problem_name,$(new_problem))),\
		$(no_force)),$(error "Solution already exists. Use f=1 variable to overwrite"))
	@echo "Creating solution for the problem $(new_problem) from template ... "
	@cat template.f08 | sed s/%N%/$(call problem_num,$(new_problem))/ \
		> $(shell printf "src/euler%03d.f08" $(new_problem))

run: lib $(shell printf "bin/euler%03d" $(run_problem))
	@echo "Running problem $(run_problem) ... "
	@./$(shell printf "bin/euler%03d" $(run_problem))

test: lib $(shell printf "bin/euler%03d" $(run_problem))
	@echo "Testing problem $(run_problem) ... "

lib:
	@$(MAKE) -C libeuler

clean:
	@$(RM) bin/*
	@$(MAKE) -C libeuler clean

bin/%: src/%.f08
	$(FC) -O3 -o $@ -Jlibeuler $< libeuler/euler_mod.o
