#!/bin/bash
echo [1/257] START
cd "/workspaces/bench-source/src/aaa"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/aaa/aaa.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [1/257] END
echo [2/257] START
cd "/workspaces/bench-source/src/ada_fuse"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/ada_fuse/ada_fuse.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [2/257] END
echo [3/257] START
cd "/workspaces/bench-source/src/ada_lua"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/ada_lua/ada_lua.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [3/257] END
echo [4/257] START
cd "/workspaces/bench-source/src/ada_pretty"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/ada_pretty/gnat/ada_pretty.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [4/257] END
echo [5/257] START
cd "/workspaces/bench-source/src/ada_toml"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/ada_toml/ada_toml.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [5/257] END
echo [6/257] START
cd "/workspaces/bench-source/src/adabots"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/adabots/adabots.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [6/257] END
echo [7/257] START
cd "/workspaces/bench-source/src/adl_middleware"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/adl_middleware/adl_middleware.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [7/257] END
echo [8/257] START
cd "/workspaces/bench-source/src/aflex"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/aflex/aflex.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [8/257] END
echo [9/257] START
cd "/workspaces/bench-source/src/agpl"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/agpl/agpl.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [9/257] END
echo [10/257] START
cd "/workspaces/bench-source/src/aicwl"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/aicwl/sources/aicwl-editor.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [10/257] END
echo [11/257] START
cd "/workspaces/bench-source/src/aicwl"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/aicwl/sources/aicwl.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [11/257] END
echo [12/257] START
cd "/workspaces/bench-source/src/ajunitgen"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/ajunitgen/ajunitgen.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [12/257] END
echo [13/257] START
cd "/workspaces/bench-source/src/anagram"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/anagram/gnat/anagram.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [13/257] END
echo [14/257] START
cd "/workspaces/bench-source/src/ansiada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/ansiada/ansiada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [14/257] END
echo [15/257] START
cd "/workspaces/bench-source/src/apdf"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/apdf/pdf_out_gnat_w_gid.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [15/257] END
echo [16/257] START
cd "/workspaces/bench-source/src/asfml"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/asfml/asfml.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [16/257] END
echo [17/257] START
cd "/workspaces/bench-source/src/atomic"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/atomic/atomic.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [17/257] END
echo [18/257] START
cd "/workspaces/bench-source/src/audio_base"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/audio_base/audio_base.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [18/257] END
echo [19/257] START
cd "/workspaces/bench-source/src/audio_wavefiles"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/audio_wavefiles/audio_wavefiles.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [19/257] END
echo [20/257] START
cd "/workspaces/bench-source/src/aunit"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/aunit/lib/gnat/aunit.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [20/257] END
echo [21/257] START
cd "/workspaces/bench-source/src/automate"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/automate/automate.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [21/257] END
echo [22/257] START
cd "/workspaces/bench-source/src/avltrees"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/avltrees/avltrees.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [22/257] END
echo [23/257] START
cd "/workspaces/bench-source/src/avrada_examples/avrada_rts"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/avrada_examples/avrada_rts/avrada_rts.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [23/257] END
echo [24/257] START
cd "/workspaces/bench-source/src/awa/ada-lzma"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/awa/ada-lzma/.alire/lzmada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [24/257] END
echo [25/257] START
cd "/workspaces/bench-source/src/awa/ada-util"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/awa/ada-util/.alire/utilada_conf.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [25/257] END
echo [26/257] START
cd "/workspaces/bench-source/src/awa/ada-util"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/awa/ada-util/utilada_base.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [26/257] END
echo [27/257] START
cd "/workspaces/bench-source/src/awa/ada-util"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/awa/ada-util/utilada_core.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [27/257] END
echo [28/257] START
cd "/workspaces/bench-source/src/awa/ada-util"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/awa/ada-util/utilada_sys.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [28/257] END
echo [29/257] START
cd "/workspaces/bench-source/src/aws"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/aws/aws.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [29/257] END
echo [30/257] START
cd "/workspaces/bench-source/src/axmpp"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/axmpp/gnat/axmpp.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [30/257] END
echo [31/257] START
cd "/workspaces/bench-source/src/ayacc"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/ayacc/ayacc.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [31/257] END
echo [32/257] START
cd "/workspaces/bench-source/src/b2ssum"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/b2ssum/b2ssum.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [32/257] END
echo [33/257] START
cd "/workspaces/bench-source/src/bar_codes"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/bar_codes/bar_codes_gnat.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [33/257] END
echo [34/257] START
cd "/workspaces/bench-source/src/basalt"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/basalt/basalt.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [34/257] END
echo [35/257] START
cd "/workspaces/bench-source/src/bbqueue"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/bbqueue/bbqueue.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [35/257] END
echo [36/257] START
cd "/workspaces/bench-source/src/bingada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/bingada/bingada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [36/257] END
echo [37/257] START
cd "/workspaces/bench-source/src/blake2s"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/blake2s/blake2s.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [37/257] END
echo [38/257] START
cd "/workspaces/bench-source/src/brackelib"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/brackelib/brackelib.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [38/257] END
echo [39/257] START
cd "/workspaces/bench-source/src/c_strings"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/c_strings/c_strings.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [39/257] END
echo [40/257] START
cd "/workspaces/bench-source/src/canberra_ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/canberra_ada/canberra_ada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [40/257] END
echo [41/257] START
cd "/workspaces/bench-source/src/cbsg"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/cbsg/cbsg.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [41/257] END
echo [42/257] START
cd "/workspaces/bench-source/src/chacha20"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/chacha20/chacha20.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [42/257] END
echo [43/257] START
cd "/workspaces/bench-source/src/chests"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/chests/chests.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [43/257] END
echo [44/257] START
cd "/workspaces/bench-source/src/clic"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/clic/clic.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [44/257] END
echo [45/257] START
cd "/workspaces/bench-source/src/cmd_ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/cmd_ada/cmd_ada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [45/257] END
echo [46/257] START
cd "/workspaces/bench-source/src/cobs"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/cobs/cobs.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [46/257] END
echo [47/257] START
cd "/workspaces/bench-source/src/dashera"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/dashera/dashera.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [47/257] END
echo [48/257] START
cd "/workspaces/bench-source/src/dcf"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/dcf/dcf/dcf.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [48/257] END
echo [49/257] START
cd "/workspaces/bench-source/src/dcf/zipdcf"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/dcf/zipdcf/zipdcf.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [49/257] END
# echo [50/257] START
# cd "/workspaces/bench-source/src/dependency_graph_extractor"
# alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/dependency_graph_extractor/dependency_graph_extractor.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
# echo [50/257] END
echo [51/257] START
cd "/workspaces/bench-source/src/dg_loada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/dg_loada/dg_loada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [51/257] END
echo [52/257] START
cd "/workspaces/bench-source/src/dir_iterators"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/dir_iterators/dir_iterators.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [52/257] END
echo [53/257] START
cd "/workspaces/bench-source/src/dotenv"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/dotenv/dotenv.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [53/257] END
echo [54/257] START
cd "/workspaces/bench-source/src/eagle_lander"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/eagle_lander/eagle_lander.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [54/257] END
echo [55/257] START
cd "/workspaces/bench-source/src/edc_client"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/edc_client/edc_client.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [55/257] END
echo [56/257] START
cd "/workspaces/bench-source/src/eeprom_i2c"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/eeprom_i2c/eeprom_i2c.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [56/257] END
echo [57/257] START
cd "/workspaces/bench-source/src/elevator"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/elevator/elevator.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [57/257] END
echo [58/257] START
cd "/workspaces/bench-source/src/emacs_gpr_query"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/emacs_gpr_query/emacs_gpr_query.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [58/257] END
echo [59/257] START
cd "/workspaces/bench-source/src/embedded_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/embedded_components/embedded_components.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [59/257] END
echo [60/257] START
cd "/workspaces/bench-source/src/emojis"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/emojis/emojis.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [60/257] END
echo [61/257] START
cd "/workspaces/bench-source/src/endianness"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/endianness/endianness.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [61/257] END
echo [62/257] START
cd "/workspaces/bench-source/src/epoll"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/epoll/epoll.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [62/257] END
echo [63/257] START
cd "/workspaces/bench-source/src/esp_idf"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/esp_idf/esp_idf.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [63/257] END
echo [64/257] START
cd "/workspaces/bench-source/src/euler_tools"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/euler_tools/euler_tools.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [64/257] END
echo [65/257] START
cd "/workspaces/bench-source/src/evdev"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/evdev/evdev_info.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [65/257] END
echo [66/257] START
cd "/workspaces/bench-source/src/evdev"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/evdev/evdev.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [66/257] END
echo [67/257] START
cd "/workspaces/bench-source/src/ews"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/ews/ews.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [67/257] END
echo [68/257] START
cd "/workspaces/bench-source/src/excel_writer"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/excel_writer/excel_out_gnat.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [68/257] END
echo [69/257] START
cd "/workspaces/bench-source/src/fastpbkdf2_ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/fastpbkdf2_ada/fastpbkdf2_ada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [69/257] END
echo [70/257] START
cd "/workspaces/bench-source/src/felix"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/felix/felix.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [70/257] END
echo [71/257] START
cd "/workspaces/bench-source/src/florist_blady"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/florist_blady/lib_florist.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [71/257] END
echo [72/257] START
cd "/workspaces/bench-source/src/freetypeada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/freetypeada/freetype.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [72/257] END
echo [73/257] START
cd "/workspaces/bench-source/src/garlic"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/garlic/gnat/garlic.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [73/257] END
echo [74/257] START
cd "/workspaces/bench-source/src/garlic/Dist"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/garlic/gnat/gnatdist.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [74/257] END
echo [75/257] START
cd "/workspaces/bench-source/src/geo_coords"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/geo_coords/geo_coords.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [75/257] END
echo [76/257] START
cd "/workspaces/bench-source/src/get_password"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/get_password/get_password.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [76/257] END
echo [77/257] START
cd "/workspaces/bench-source/src/getopt"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/getopt/getopt.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [77/257] END
echo [78/257] START
cd "/workspaces/bench-source/src/gid"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gid/gid.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [78/257] END
echo [79/257] START
cd "/workspaces/bench-source/src/gnat_math_extensions"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnat_math_extensions/gnat_math_extensions.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [79/257] END
echo [80/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/gmp"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-bindings/gmp/gnatcoll_gmp.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [80/257] END
echo [81/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/iconv"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-bindings/iconv/gnatcoll_iconv.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [81/257] END
echo [82/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/lzma"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-bindings/lzma/gnatcoll_lzma.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [82/257] END
echo [83/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/omp"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-bindings/omp/gnatcoll_omp.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [83/257] END
echo [84/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/python"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-bindings/python/gnatcoll_python.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [84/257] END
echo [85/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/readline"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-bindings/readline/gnatcoll_readline.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [85/257] END
echo [86/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/syslog"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-bindings/syslog/gnatcoll_syslog.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [86/257] END
echo [87/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/zlib"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-bindings/zlib/gnatcoll_zlib.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [87/257] END
echo [88/257] START
cd "/workspaces/bench-source/src/gnatcoll-core"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-core/gnatcoll.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [88/257] END
echo [89/257] START
cd "/workspaces/bench-source/src/gnatcoll-db/postgres"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-db/postgres/gnatcoll_postgres.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [89/257] END
echo [90/257] START
cd "/workspaces/bench-source/src/gnatcoll-db/sql"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-db/sql/gnatcoll_sql.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [90/257] END
echo [91/257] START
cd "/workspaces/bench-source/src/gnatcoll-db/sqlite"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-db/sqlite/gnatcoll_sqlite.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [91/257] END
echo [92/257] START
cd "/workspaces/bench-source/src/gnatcoll-db/xref"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gnatcoll-db/xref/gnatcoll_xref.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [92/257] END
echo [93/257] START
cd "/workspaces/bench-source/src/gpr_unit_provider"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gpr_unit_provider/gpr_unit_provider.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [93/257] END
echo [94/257] START
cd "/workspaces/bench-source/src/gprbuild"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gprbuild/gpr/gpr.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [94/257] END
echo [95/257] START
cd "/workspaces/bench-source/src/gtkada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/gtkada/src/gtkada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [95/257] END
echo [96/257] START
cd "/workspaces/bench-source/src/hac"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/hac/hac.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [96/257] END
echo [97/257] START
cd "/workspaces/bench-source/src/hal"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/hal/hal.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [97/257] END
echo [98/257] START
cd "/workspaces/bench-source/src/hangman"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/hangman/hangman.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [98/257] END
echo [99/257] START
cd "/workspaces/bench-source/src/hello"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/hello/hello.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [99/257] END
echo [100/257] START
cd "/workspaces/bench-source/src/hmac"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/hmac/hmac.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [100/257] END
echo [101/257] START
cd "/workspaces/bench-source/src/honki_tonks_zivilisationen"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/honki_tonks_zivilisationen/honki_tonks_zivilisationen.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [101/257] END
echo [102/257] START
cd "/workspaces/bench-source/src/hungarian"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/hungarian/hungarian.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [102/257] END
echo [103/257] START
cd "/workspaces/bench-source/src/ini_files"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/ini_files/ini_files.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [103/257] END
echo [104/257] START
cd "/workspaces/bench-source/src/inotify"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/inotify/inotify.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [104/257] END
echo [105/257] START
cd "/workspaces/bench-source/src/inotify"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/inotify/monitor.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [105/257] END
echo [106/257] START
cd "/workspaces/bench-source/src/j2ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/j2ada/j2ada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [106/257] END
echo [107/257] START
cd "/workspaces/bench-source/src/json/json"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/json/json/json_pretty_print.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [107/257] END
echo [108/257] START
cd "/workspaces/bench-source/src/json/json"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/json/json/json.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [108/257] END
echo [109/257] START
cd "/workspaces/bench-source/src/jupyter_kernel"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/jupyter_kernel/gnat/jupyter_ada_driver.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [109/257] END
echo [110/257] START
cd "/workspaces/bench-source/src/jupyter_kernel"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/jupyter_kernel/gnat/jupyter_ada_kernel.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [110/257] END
echo [111/257] START
cd "/workspaces/bench-source/src/jwt"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/jwt/gnat/jwt.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [111/257] END
echo [112/257] START
cd "/workspaces/bench-source/src/labs_radar"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/labs_radar/labs_radar.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [112/257] END
echo [113/257] START
cd "/workspaces/bench-source/src/labs_radar/labs_standalone"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/labs_radar/labs_standalone/labs_standalone.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [113/257] END
echo [114/257] START
cd "/workspaces/bench-source/src/lace"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace/library/lace.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [114/257] END
echo [115/257] START
cd "/workspaces/bench-source/src/lace_box2d"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace_box2d/library/box2d_thin.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [115/257] END
echo [116/257] START
cd "/workspaces/bench-source/src/lace_bullet"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace_bullet/library/bullet_thin.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [116/257] END
echo [117/257] START
cd "/workspaces/bench-source/src/lace_c_math"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace_c_math/library/c_math_thin.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [117/257] END
echo [118/257] START
cd "/workspaces/bench-source/src/lace_gel_animation_demo"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace_gel_animation_demo/human_rig_demo.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [118/257] END
echo [119/257] START
cd "/workspaces/bench-source/src/lace_gel_full_demo"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace_gel_full_demo/full_demo.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [119/257] END
echo [120/257] START
cd "/workspaces/bench-source/src/lace_shared"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace_shared/lace_shared.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [120/257] END
echo [121/257] START
cd "/workspaces/bench-source/src/lace/lace_collada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace/lace_collada/library/collada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [121/257] END
echo [122/257] START
cd "/workspaces/bench-source/src/lace/lace_gel"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace/lace_gel/library/gel.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [122/257] END
echo [123/257] START
cd "/workspaces/bench-source/src/lace/lace_math"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace/lace_math/library/math.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [123/257] END
echo [124/257] START
cd "/workspaces/bench-source/src/lace/lace_opengl"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace/lace_opengl/library/opengl.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [124/257] END
echo [125/257] START
cd "/workspaces/bench-source/src/lace/lace_physics"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace/lace_physics/library/physics.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [125/257] END
echo [126/257] START
cd "/workspaces/bench-source/src/lace/lace_swig"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace/lace_swig/library/swig.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [126/257] END
echo [127/257] START
cd "/workspaces/bench-source/src/lace/lace_xml"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lace/lace_xml/library/xml.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [127/257] END
echo [128/257] START
cd "/workspaces/bench-source/src/lal_highlight"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lal_highlight/highlight.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [128/257] END
echo [129/257] START
cd "/workspaces/bench-source/src/langkit_support"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/langkit_support/langkit_support.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [129/257] END
echo [130/257] START
cd "/workspaces/bench-source/src/libadalang"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/libadalang/libadalang.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [130/257] END
echo [131/257] START
cd "/workspaces/bench-source/src/libadalang2xml"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/libadalang2xml/libadalang2xml.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [131/257] END
echo [132/257] START
cd "/workspaces/bench-source/src/libgpr2"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/libgpr2/gpr2.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [132/257] END
echo [133/257] START
cd "/workspaces/bench-source/src/libgpr2/gpr2"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/libgpr2/gpr2.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [133/257] END
echo [134/257] START
cd "/workspaces/bench-source/src/libhello"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/libhello/libhello.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [134/257] END
echo [135/257] START
cd "/workspaces/bench-source/src/libkeccak"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/libkeccak/libkeccak.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [135/257] END
echo [136/257] START
cd "/workspaces/bench-source/src/libsimpleio"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/libsimpleio/libsimpleio.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [136/257] END
echo [137/257] START
cd "/workspaces/bench-source/src/linenoise_ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/linenoise_ada/linenoise.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [137/257] END
echo [138/257] START
cd "/workspaces/bench-source/src/littlefs"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/littlefs/littlefs.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [138/257] END
echo [139/257] START
cd "/workspaces/bench-source/src/lmdb_ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lmdb_ada/lmdb.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [139/257] END
echo [140/257] START
cd "/workspaces/bench-source/src/loga"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/loga/loga.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [140/257] END
echo [141/257] START
cd "/workspaces/bench-source/src/lvgl_ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lvgl_ada/lvgl_ada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [141/257] END
echo [142/257] START
cd "/workspaces/bench-source/src/lvgl_ada/lvgl_ada_simulator"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/lvgl_ada/lvgl_ada_simulator/lvgl_ada_simulator.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [142/257] END
echo [143/257] START
cd "/workspaces/bench-source/src/mage"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/mage/mage.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [143/257] END
echo [144/257] START
cd "/workspaces/bench-source/src/mage_hat"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/mage_hat/mage_hat.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [144/257] END
echo [145/257] START
cd "/workspaces/bench-source/src/mandelbrot_ascii"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/mandelbrot_ascii/mandelbrot_ascii.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [145/257] END
echo [146/257] START
cd "/workspaces/bench-source/src/markdown"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/markdown/gnat/markdown.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [146/257] END
echo [147/257] START
cd "/workspaces/bench-source/src/mathpaqs"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/mathpaqs/mathpaqs.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [147/257] END
echo [148/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_amf"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_amf.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [148/257] END
echo [149/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_amf_dd"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_amf_dd.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [149/257] END
echo [150/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_fastcgi"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_fastcgi.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [150/257] END
echo [151/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_league"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/packages/alire/matreshka_league/build_matreshka_league.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [151/257] END
echo [152/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_servlet"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_servlet.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [152/257] END
echo [153/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_soap"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_soap.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [153/257] END
echo [154/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_soap_wsse"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_soap_wsse.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [154/257] END
echo [155/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_spikedog_api"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_spikedog_api.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [155/257] END
echo [156/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_spikedog_core"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_spikedog_core.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [156/257] END
echo [157/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_sql"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_sql.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [157/257] END
echo [158/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_sql_firebird"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_sql_firebird.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [158/257] END
echo [159/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_sql_mysql"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_sql_mysql.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [159/257] END
echo [160/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_sql_postgresql"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_sql_postgresql.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [160/257] END
echo [161/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_sql_sqlite3"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/gnat/matreshka_sql_sqlite3.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [161/257] END
echo [162/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_xml"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/matreshka/packages/alire/matreshka_xml/build_matreshka_xml.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [162/257] END
echo [163/257] START
cd "/workspaces/bench-source/src/mcp2221"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/mcp2221/mcp2221.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [163/257] END
echo [164/257] START
cd "/workspaces/bench-source/src/midi"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/midi/midi.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [164/257] END
echo [165/257] START
cd "/workspaces/bench-source/src/minirest"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/minirest/minirest.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [165/257] END
echo [166/257] START
cd "/workspaces/bench-source/src/ncursesada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/ncursesada/Ada95/lib_adacurses.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [166/257] END
echo [167/257] START
cd "/workspaces/bench-source/src/openglada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/openglada/opengl.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [167/257] END
echo [168/257] START
cd "/workspaces/bench-source/src/openglada_glfw"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/openglada_glfw/opengl-glfw.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [168/257] END
echo [169/257] START
cd "/workspaces/bench-source/src/openglada_images"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/openglada_images/opengl-images.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [169/257] END
echo [170/257] START
cd "/workspaces/bench-source/src/openglada/openglada_text"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/openglada/openglada_text/opengl-text.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [170/257] END
echo [171/257] START
cd "/workspaces/bench-source/src/optional"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/optional/optional.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [171/257] END
echo [172/257] START
cd "/workspaces/bench-source/src/parse_args"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/parse_args/parse_args.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [172/257] END
echo [173/257] START
cd "/workspaces/bench-source/src/partord"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/partord/partord.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [173/257] END
echo [174/257] START
cd "/workspaces/bench-source/src/pbkdf2"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/pbkdf2/pbkdf2.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [174/257] END
echo [175/257] START
cd "/workspaces/bench-source/src/play_2048"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/play_2048/play_2048.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [175/257] END
echo [176/257] START
cd "/workspaces/bench-source/src/powerjoular"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/powerjoular/powerjoular.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [176/257] END
echo [177/257] START
cd "/workspaces/bench-source/src/progress_indicators"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/progress_indicators/progress_indicators.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [177/257] END
echo [178/257] START
cd "/workspaces/bench-source/src/protobuf"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/protobuf/gnat/protoc_gen_ada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [178/257] END
echo [179/257] START
cd "/workspaces/bench-source/src/qoi"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/qoi/qoi.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [179/257] END
echo [180/257] START
cd "/workspaces/bench-source/src/raspberry_bsp"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/raspberry_bsp/raspberry_bsp.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [180/257] END
echo [181/257] START
cd "/workspaces/bench-source/src/rejuvenation"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/rejuvenation/rejuvenation.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [181/257] END
echo [182/257] START
cd "/workspaces/bench-source/src/remoteio"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/remoteio/remoteio.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [182/257] END
echo [183/257] START
cd "/workspaces/bench-source/src/resources"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/resources/resources.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [183/257] END
echo [184/257] START
cd "/workspaces/bench-source/src/rewriters"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/rewriters/rewriters.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [184/257] END
echo [185/257] START
cd "/workspaces/bench-source/src/rsfile"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/rsfile/rsfile.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [185/257] END
echo [186/257] START
cd "/workspaces/bench-source/src/rtmidi"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/rtmidi/rtmidi.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [186/257] END
echo [187/257] START
cd "/workspaces/bench-source/src/rxada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/rxada/rxada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [187/257] END
echo [188/257] START
cd "/workspaces/bench-source/src/saatana"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/saatana/saatana.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [188/257] END
echo [189/257] START
cd "/workspaces/bench-source/src/scripted_testing"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/scripted_testing/scripted_testing.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [189/257] END
echo [190/257] START
cd "/workspaces/bench-source/src/sdlada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/sdlada/build/gnat/sdlada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [190/257] END
echo [191/257] START
cd "/workspaces/bench-source/src/semantic_versioning"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/semantic_versioning/semantic_versioning.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [191/257] END
echo [192/257] START
cd "/workspaces/bench-source/src/septum"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/septum/septum.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [192/257] END
echo [193/257] START
cd "/workspaces/bench-source/src/sha1"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/sha1/sha1.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [193/257] END
echo [194/257] START
cd "/workspaces/bench-source/src/sha2"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/sha2/sha2.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [194/257] END
echo [195/257] START
cd "/workspaces/bench-source/src/si_units"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/si_units/si_units.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [195/257] END
echo [196/257] START
cd "/workspaces/bench-source/src/simh_tapes"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simh_tapes/simh_tapes.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [196/257] END
echo [197/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components-connections_server-elv_max_cube.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [197/257] END
echo [198/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components-connections_server-http_server-sqlite_browser.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [198/257] END
echo [199/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components-connections_server-http_server.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [199/257] END
echo [200/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components-connections_server-modbus.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [200/257] END
echo [201/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components-connections_server-mqtt.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [201/257] END
echo [202/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components-connections_server-secure.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [202/257] END
echo [203/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components-connections_server-smtp.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [203/257] END
echo [204/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components-connections_server.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [204/257] END
echo [205/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components-gnutls.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [205/257] END
echo [206/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components-ntp.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [206/257] END
echo [207/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components-sqlite.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [207/257] END
echo [208/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/components.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [208/257] END
echo [209/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/strings_edit.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [209/257] END
echo [210/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_components/tables.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [210/257] END
echo [211/257] START
cd "/workspaces/bench-source/src/simple_logging"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/simple_logging/simple_logging.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [211/257] END
echo [212/257] START
cd "/workspaces/bench-source/src/slip"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/slip/slip.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [212/257] END
echo [213/257] START
cd "/workspaces/bench-source/src/socketcan"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/socketcan/src/socketcan.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [213/257] END
echo [214/257] START
cd "/workspaces/bench-source/src/spark_unbound"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/spark_unbound/spark_unbound.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [214/257] END
echo [215/257] START
cd "/workspaces/bench-source/src/sparknacl"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/sparknacl/sparknacl.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [215/257] END
echo [216/257] START
cd "/workspaces/bench-source/src/spat"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/spat/spat.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [216/257] END
echo [217/257] START
cd "/workspaces/bench-source/src/spdx"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/spdx/spdx.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [217/257] END
# echo [218/257] START
# cd "/workspaces/bench-source/src/specfun"
# alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/specfun/specfun.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
# echo [218/257] END
echo [219/257] START
cd "/workspaces/bench-source/src/spoon"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/spoon/spoon.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [219/257] END
echo [220/257] START
cd "/workspaces/bench-source/src/startup_gen"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/startup_gen/startup_gen.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [220/257] END
echo [221/257] START
cd "/workspaces/bench-source/src/stephes_ada_library"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/stephes_ada_library/build/stephes_ada_library.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [221/257] END
echo [222/257] START
cd "/workspaces/bench-source/src/stopwatch"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/stopwatch/stopwatch.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [222/257] END
echo [223/257] START
cd "/workspaces/bench-source/src/svd2ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/svd2ada/svd2ada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [223/257] END
echo [224/257] START
cd "/workspaces/bench-source/src/system_random"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/system_random/system_random.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [224/257] END
echo [225/257] START
cd "/workspaces/bench-source/src/tash"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/tash/tash.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [225/257] END
echo [226/257] START
cd "/workspaces/bench-source/src/templates_parser"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/templates_parser/templates_parser.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [226/257] END
echo [227/257] START
cd "/workspaces/bench-source/src/tiled_code_gen"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/tiled_code_gen/tiled_code_gen.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [227/257] END
echo [228/257] START
cd "/workspaces/bench-source/src/tiny_text"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/tiny_text/tiny_text.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [228/257] END
echo [229/257] START
cd "/workspaces/bench-source/src/tlsada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/tlsada/tlsada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [229/257] END
echo [230/257] START
cd "/workspaces/bench-source/src/toml_slicer"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/toml_slicer/toml_slicer.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [230/257] END
echo [231/257] START
cd "/workspaces/bench-source/src/trendy_terminal"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/trendy_terminal/trendy_terminal.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [231/257] END
echo [232/257] START
cd "/workspaces/bench-source/src/trendy_test"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/trendy_test/trendy_test.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [232/257] END
echo [233/257] START
cd "/workspaces/bench-source/src/uri_ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/uri_ada/uri_ada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [233/257] END
echo [234/257] START
cd "/workspaces/bench-source/src/uri_mime"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/uri_mime/uri_mime.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [234/257] END
echo [235/257] START
cd "/workspaces/bench-source/src/usb_embedded"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/usb_embedded/usb_embedded.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [235/257] END
echo [236/257] START
cd "/workspaces/bench-source/src/utf8test"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/utf8test/utf8test.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [236/257] END
echo [237/257] START
cd "/workspaces/bench-source/src/vaton"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/vaton/vaton.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [237/257] END
echo [238/257] START
cd "/workspaces/bench-source/src/virtapu"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/virtapu/virtapu.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [238/257] END
echo [239/257] START
cd "/workspaces/bench-source/src/weechat_ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/weechat_ada/weechat_ada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [239/257] END
echo [240/257] START
cd "/workspaces/bench-source/src/wordle"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/wordle/wordle.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [240/257] END
echo [241/257] START
cd "/workspaces/bench-source/src/wordlelib"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/wordlelib/wordlelib.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [241/257] END
echo [242/257] START
cd "/workspaces/bench-source/src/wordlist"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/wordlist/wordlist.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [242/257] END
echo [243/257] START
cd "/workspaces/bench-source/src/workers"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/workers/workers.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [243/257] END
echo [244/257] START
cd "/workspaces/bench-source/src/xdg_base_dir"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/xdg_base_dir/xdg_base_dir.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [244/257] END
echo [245/257] START
cd "/workspaces/bench-source/src/xia"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/xia/XIA.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [245/257] END
echo [246/257] START
cd "/workspaces/bench-source/src/xml_ez_out"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/xml_ez_out/xml_ez_out.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [246/257] END
echo [247/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/xmlada/distrib/xmlada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [247/257] END
echo [248/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/xmlada/dom/xmlada_dom.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [248/257] END
echo [249/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/xmlada/input_sources/xmlada_input.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [249/257] END
echo [250/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/xmlada/sax/xmlada_sax.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [250/257] END
echo [251/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/xmlada/schema/xmlada_schema.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [251/257] END
echo [252/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/xmlada/unicode/xmlada_unicode.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [252/257] END
echo [253/257] START
cd "/workspaces/bench-source/src/xoshiro"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/xoshiro/xoshiro.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [253/257] END
echo [254/257] START
cd "/workspaces/bench-source/src/yeison"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/yeison/yeison.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [254/257] END
echo [255/257] START
cd "/workspaces/bench-source/src/zeromq_ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/zeromq_ada/zmq.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [255/257] END
echo [256/257] START
cd "/workspaces/bench-source/src/zipada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/zipada/zipada.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [256/257] END
echo [257/257] START
cd "/workspaces/bench-source/src/zlib_ada"
alr exec -- gnatcheck -q -t -l --show-rule -o gnatcheck.report -P/workspaces/bench-source/src/zlib_ada/zlib.gpr -rules -from=/workspaces/bench-source/benchmark-rules/all_rules_in_one_file/gnatcheck.rules
echo [257/257] END
