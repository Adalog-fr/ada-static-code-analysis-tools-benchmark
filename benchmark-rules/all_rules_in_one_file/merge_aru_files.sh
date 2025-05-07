#!/bin/bash

echo "set verbose on;\nset timing global;\nset statistics 3;\n" > _all.aru
cat ../*.aru > _all.aru
