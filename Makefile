# Makefile для Haskell проекта

# Компилятор
GHC := ghc

# Имя исполняемого файла
TARGET := main.executable

# Автоматическое определение всех .hs файлов в текущей директории
HS_SOURCES := $(wildcard *.hs)

# Если нет .hs файлов, используем main.hs по умолчанию
ifeq ($(HS_SOURCES),)
HS_SOURCES := main.hs
endif

# Основной исходный файл (первый из списка или main.hs)
MAIN_SOURCE := $(firstword $(HS_SOURCES))

# Флаги компиляции
GHC_FLAGS := -O2 -Wall

# Файлы для очистки
CLEAN_FILES := *.hi *.o $(TARGET)

.PHONY: all run clean rebuild info

all: $(TARGET)

$(TARGET): $(HS_SOURCES)
	$(GHC) $(GHC_FLAGS) --make $(MAIN_SOURCE) -o $@

run: $(TARGET)
	./$(TARGET)


clean:
	rm -f $(CLEAN_FILES)

rebuild: clean all

info:
	@echo "Source files: $(HS_SOURCES)"
	@echo "Main source: $(MAIN_SOURCE)"
	@echo "Target: $(TARGET)"