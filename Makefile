ELPA_DEPENDENCIES=package-lint s dash f ert-async el-mock shut-up ecukes ansi espuds

ELPA_ARCHIVES=melpa-stable gnu

TEST_ECUKES_FEATURE_FILES=$(wildcard features/*.feature)
TEST_ERT_FILES=$(wildcard test/*.el)
LINT_CHECKDOC_FILES=$(wildcard *.el)
LINT_PACKAGE_LINT_FILES=$(wildcard *.el)
LINT_COMPILE_FILES=$(wildcard *.el)

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://gitlab.petton.fr/DamienCassou/makel/raw/v0.7.1/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
