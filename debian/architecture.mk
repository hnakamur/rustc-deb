# This Makefile snippet defines DEB_*_RUST_TYPE triples based on DEB_*_GNU_TYPE

include /usr/share/dpkg/architecture.mk

rust_cpu = $(subst i586,i686,$(1))

$(foreach machine,BUILD HOST TARGET,\
  $(eval DEB_$(machine)_RUST_TYPE ?= $(call rust_cpu,$(DEB_$(machine)_GNU_CPU))-unknown-$(DEB_$(machine)_GNU_SYSTEM)))
