#!/bin/bash

echo [1/257] START
cd "/workspaces/bench-source/src/aaa"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/aaa/aaa.gpr @/workspaces/bench-source/src/aaa/aaa.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [1/257] END
echo [2/257] START
cd "/workspaces/bench-source/src/ada_fuse"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/ada_fuse/ada_fuse.gpr @/workspaces/bench-source/src/ada_fuse/ada_fuse.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [2/257] END
echo [3/257] START
cd "/workspaces/bench-source/src/ada_lua"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/ada_lua/ada_lua.gpr @/workspaces/bench-source/src/ada_lua/ada_lua.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [3/257] END
echo [4/257] START
cd "/workspaces/bench-source/src/ada_pretty"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/ada_pretty/gnat/ada_pretty.gpr @/workspaces/bench-source/src/ada_pretty/gnat/ada_pretty.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [4/257] END
echo [5/257] START
cd "/workspaces/bench-source/src/ada_toml"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/ada_toml/ada_toml.gpr @/workspaces/bench-source/src/ada_toml/ada_toml.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [5/257] END
echo [6/257] START
cd "/workspaces/bench-source/src/adabots"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/adabots/adabots.gpr @/workspaces/bench-source/src/adabots/adabots.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [6/257] END
echo [7/257] START
cd "/workspaces/bench-source/src/adl_middleware"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/adl_middleware/adl_middleware.gpr @/workspaces/bench-source/src/adl_middleware/adl_middleware.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [7/257] END
echo [8/257] START
cd "/workspaces/bench-source/src/aflex"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/aflex/aflex.gpr @/workspaces/bench-source/src/aflex/aflex.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [8/257] END
echo [9/257] START
cd "/workspaces/bench-source/src/agpl"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/agpl/agpl.gpr @/workspaces/bench-source/src/agpl/agpl.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [9/257] END
echo [10/257] START
cd "/workspaces/bench-source/src/aicwl"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/aicwl/sources/aicwl-editor.gpr @/workspaces/bench-source/src/aicwl/sources/aicwl-editor.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [10/257] END
echo [11/257] START
cd "/workspaces/bench-source/src/aicwl"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/aicwl/sources/aicwl.gpr @/workspaces/bench-source/src/aicwl/sources/aicwl.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [11/257] END
echo [12/257] START
cd "/workspaces/bench-source/src/ajunitgen"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/ajunitgen/ajunitgen.gpr @/workspaces/bench-source/src/ajunitgen/ajunitgen.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [12/257] END
echo [13/257] START
cd "/workspaces/bench-source/src/anagram"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/anagram/gnat/anagram.gpr @/workspaces/bench-source/src/anagram/gnat/anagram.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [13/257] END
echo [14/257] START
cd "/workspaces/bench-source/src/ansiada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/ansiada/ansiada.gpr @/workspaces/bench-source/src/ansiada/ansiada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [14/257] END
echo [15/257] START
cd "/workspaces/bench-source/src/apdf"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/apdf/pdf_out_gnat_w_gid.gpr @/workspaces/bench-source/src/apdf/pdf_out_gnat_w_gid.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [15/257] END
echo [16/257] START
cd "/workspaces/bench-source/src/asfml"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/asfml/asfml.gpr @/workspaces/bench-source/src/asfml/asfml.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [16/257] END
echo [17/257] START
cd "/workspaces/bench-source/src/atomic"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/atomic/atomic.gpr @/workspaces/bench-source/src/atomic/atomic.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [17/257] END
echo [18/257] START
cd "/workspaces/bench-source/src/audio_base"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/audio_base/audio_base.gpr @/workspaces/bench-source/src/audio_base/audio_base.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [18/257] END
echo [19/257] START
cd "/workspaces/bench-source/src/audio_wavefiles"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/audio_wavefiles/audio_wavefiles.gpr @/workspaces/bench-source/src/audio_wavefiles/audio_wavefiles.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [19/257] END
echo [20/257] START
cd "/workspaces/bench-source/src/aunit"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/aunit/lib/gnat/aunit.gpr @/workspaces/bench-source/src/aunit/lib/gnat/aunit.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [20/257] END
echo [21/257] START
cd "/workspaces/bench-source/src/automate"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/automate/automate.gpr @/workspaces/bench-source/src/automate/automate.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [21/257] END
echo [22/257] START
cd "/workspaces/bench-source/src/avltrees"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/avltrees/avltrees.gpr @/workspaces/bench-source/src/avltrees/avltrees.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [22/257] END
# echo [23/257] START
# cd "/workspaces/bench-source/src/avrada_examples/avrada_rts"
# alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/avrada_examples/avrada_rts/avrada_rts.gpr @/workspaces/bench-source/src/avrada_examples/avrada_rts/avrada_rts.units &>> /workspaces/bench-source/Adactl_benchmark.output
# echo [23/257] END
echo [24/257] START
cd "/workspaces/bench-source/src/awa/ada-lzma"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/awa/ada-lzma/.alire/lzmada.gpr @/workspaces/bench-source/src/awa/ada-lzma/.alire/lzmada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [24/257] END
echo [25/257] START
cd "/workspaces/bench-source/src/awa/ada-util"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/awa/ada-util/.alire/utilada_conf.gpr @/workspaces/bench-source/src/awa/ada-util/.alire/utilada_conf.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [25/257] END
echo [26/257] START
cd "/workspaces/bench-source/src/awa/ada-util"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/awa/ada-util/utilada_base.gpr @/workspaces/bench-source/src/awa/ada-util/utilada_base.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [26/257] END
echo [27/257] START
cd "/workspaces/bench-source/src/awa/ada-util"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/awa/ada-util/utilada_core.gpr @/workspaces/bench-source/src/awa/ada-util/utilada_core.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [27/257] END
echo [28/257] START
cd "/workspaces/bench-source/src/awa/ada-util"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/awa/ada-util/utilada_sys.gpr @/workspaces/bench-source/src/awa/ada-util/utilada_sys.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [28/257] END
echo [29/257] START
cd "/workspaces/bench-source/src/aws"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/aws/aws.gpr @/workspaces/bench-source/src/aws/aws.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [29/257] END
echo [30/257] START
cd "/workspaces/bench-source/src/axmpp"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/axmpp/gnat/axmpp.gpr @/workspaces/bench-source/src/axmpp/gnat/axmpp.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [30/257] END
echo [31/257] START
cd "/workspaces/bench-source/src/ayacc"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/ayacc/ayacc.gpr @/workspaces/bench-source/src/ayacc/ayacc.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [31/257] END
echo [32/257] START
cd "/workspaces/bench-source/src/b2ssum"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/b2ssum/b2ssum.gpr @/workspaces/bench-source/src/b2ssum/b2ssum.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [32/257] END
echo [33/257] START
cd "/workspaces/bench-source/src/bar_codes"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/bar_codes/bar_codes_gnat.gpr @/workspaces/bench-source/src/bar_codes/bar_codes_gnat.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [33/257] END
echo [34/257] START
cd "/workspaces/bench-source/src/basalt"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/basalt/basalt.gpr @/workspaces/bench-source/src/basalt/basalt.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [34/257] END
echo [35/257] START
cd "/workspaces/bench-source/src/bbqueue"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/bbqueue/bbqueue.gpr @/workspaces/bench-source/src/bbqueue/bbqueue.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [35/257] END
echo [36/257] START
cd "/workspaces/bench-source/src/bingada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/bingada/bingada.gpr @/workspaces/bench-source/src/bingada/bingada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [36/257] END
echo [37/257] START
cd "/workspaces/bench-source/src/blake2s"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/blake2s/blake2s.gpr @/workspaces/bench-source/src/blake2s/blake2s.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [37/257] END
echo [38/257] START
cd "/workspaces/bench-source/src/brackelib"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/brackelib/brackelib.gpr @/workspaces/bench-source/src/brackelib/brackelib.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [38/257] END
echo [39/257] START
cd "/workspaces/bench-source/src/c_strings"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/c_strings/c_strings.gpr @/workspaces/bench-source/src/c_strings/c_strings.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [39/257] END
echo [40/257] START
cd "/workspaces/bench-source/src/canberra_ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/canberra_ada/canberra_ada.gpr @/workspaces/bench-source/src/canberra_ada/canberra_ada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [40/257] END
echo [41/257] START
cd "/workspaces/bench-source/src/cbsg"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/cbsg/cbsg.gpr @/workspaces/bench-source/src/cbsg/cbsg.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [41/257] END
echo [42/257] START
cd "/workspaces/bench-source/src/chacha20"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/chacha20/chacha20.gpr @/workspaces/bench-source/src/chacha20/chacha20.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [42/257] END
echo [43/257] START
cd "/workspaces/bench-source/src/chests"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/chests/chests.gpr @/workspaces/bench-source/src/chests/chests.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [43/257] END
echo [44/257] START
cd "/workspaces/bench-source/src/clic"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/clic/clic.gpr @/workspaces/bench-source/src/clic/clic.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [44/257] END
echo [45/257] START
cd "/workspaces/bench-source/src/cmd_ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/cmd_ada/cmd_ada.gpr @/workspaces/bench-source/src/cmd_ada/cmd_ada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [45/257] END
echo [46/257] START
cd "/workspaces/bench-source/src/cobs"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/cobs/cobs.gpr @/workspaces/bench-source/src/cobs/cobs.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [46/257] END
echo [47/257] START
cd "/workspaces/bench-source/src/dashera"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/dashera/dashera.gpr @/workspaces/bench-source/src/dashera/dashera.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [47/257] END
echo [48/257] START
cd "/workspaces/bench-source/src/dcf"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/dcf/dcf/dcf.gpr @/workspaces/bench-source/src/dcf/dcf/dcf.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [48/257] END
echo [49/257] START
cd "/workspaces/bench-source/src/dcf/zipdcf"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/dcf/zipdcf/zipdcf.gpr @/workspaces/bench-source/src/dcf/zipdcf/zipdcf.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [49/257] END
# echo [50/257] START
# cd "/workspaces/bench-source/src/dependency_graph_extractor"
# alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/dependency_graph_extractor/dependency_graph_extractor.gpr @/workspaces/bench-source/src/dependency_graph_extractor/dependency_graph_extractor.units &>> /workspaces/bench-source/Adactl_benchmark.output
# echo [50/257] END
echo [51/257] START
cd "/workspaces/bench-source/src/dg_loada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/dg_loada/dg_loada.gpr @/workspaces/bench-source/src/dg_loada/dg_loada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [51/257] END
echo [52/257] START
cd "/workspaces/bench-source/src/dir_iterators"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/dir_iterators/dir_iterators.gpr @/workspaces/bench-source/src/dir_iterators/dir_iterators.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [52/257] END
echo [53/257] START
cd "/workspaces/bench-source/src/dotenv"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/dotenv/dotenv.gpr @/workspaces/bench-source/src/dotenv/dotenv.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [53/257] END
echo [54/257] START
cd "/workspaces/bench-source/src/eagle_lander"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/eagle_lander/eagle_lander.gpr @/workspaces/bench-source/src/eagle_lander/eagle_lander.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [54/257] END
echo [55/257] START
cd "/workspaces/bench-source/src/edc_client"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/edc_client/edc_client.gpr @/workspaces/bench-source/src/edc_client/edc_client.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [55/257] END
echo [56/257] START
cd "/workspaces/bench-source/src/eeprom_i2c"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/eeprom_i2c/eeprom_i2c.gpr @/workspaces/bench-source/src/eeprom_i2c/eeprom_i2c.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [56/257] END
echo [57/257] START
cd "/workspaces/bench-source/src/elevator"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/elevator/elevator.gpr @/workspaces/bench-source/src/elevator/elevator.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [57/257] END
echo [58/257] START
cd "/workspaces/bench-source/src/emacs_gpr_query"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/emacs_gpr_query/emacs_gpr_query.gpr @/workspaces/bench-source/src/emacs_gpr_query/emacs_gpr_query.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [58/257] END
echo [59/257] START
cd "/workspaces/bench-source/src/embedded_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/embedded_components/embedded_components.gpr @/workspaces/bench-source/src/embedded_components/embedded_components.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [59/257] END
echo [60/257] START
cd "/workspaces/bench-source/src/emojis"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/emojis/emojis.gpr @/workspaces/bench-source/src/emojis/emojis.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [60/257] END
echo [61/257] START
cd "/workspaces/bench-source/src/endianness"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/endianness/endianness.gpr @/workspaces/bench-source/src/endianness/endianness.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [61/257] END
echo [62/257] START
cd "/workspaces/bench-source/src/epoll"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/epoll/epoll.gpr @/workspaces/bench-source/src/epoll/epoll.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [62/257] END
echo [63/257] START
cd "/workspaces/bench-source/src/esp_idf"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/esp_idf/esp_idf.gpr @/workspaces/bench-source/src/esp_idf/esp_idf.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [63/257] END
echo [64/257] START
cd "/workspaces/bench-source/src/euler_tools"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/euler_tools/euler_tools.gpr @/workspaces/bench-source/src/euler_tools/euler_tools.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [64/257] END
echo [65/257] START
cd "/workspaces/bench-source/src/evdev"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/evdev/evdev_info.gpr @/workspaces/bench-source/src/evdev/evdev_info.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [65/257] END
echo [66/257] START
cd "/workspaces/bench-source/src/evdev"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/evdev/evdev.gpr @/workspaces/bench-source/src/evdev/evdev.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [66/257] END
echo [67/257] START
cd "/workspaces/bench-source/src/ews"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/ews/ews.gpr @/workspaces/bench-source/src/ews/ews.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [67/257] END
echo [68/257] START
cd "/workspaces/bench-source/src/excel_writer"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/excel_writer/excel_out_gnat.gpr @/workspaces/bench-source/src/excel_writer/excel_out_gnat.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [68/257] END
echo [69/257] START
cd "/workspaces/bench-source/src/fastpbkdf2_ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/fastpbkdf2_ada/fastpbkdf2_ada.gpr @/workspaces/bench-source/src/fastpbkdf2_ada/fastpbkdf2_ada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [69/257] END
echo [70/257] START
cd "/workspaces/bench-source/src/felix"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/felix/felix.gpr @/workspaces/bench-source/src/felix/felix.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [70/257] END
echo [71/257] START
cd "/workspaces/bench-source/src/florist_blady"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/florist_blady/lib_florist.gpr @/workspaces/bench-source/src/florist_blady/lib_florist.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [71/257] END
echo [72/257] START
cd "/workspaces/bench-source/src/freetypeada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/freetypeada/freetype.gpr @/workspaces/bench-source/src/freetypeada/freetype.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [72/257] END
echo [73/257] START
cd "/workspaces/bench-source/src/garlic"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/garlic/gnat/garlic.gpr @/workspaces/bench-source/src/garlic/gnat/garlic.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [73/257] END
echo [74/257] START
cd "/workspaces/bench-source/src/garlic/Dist"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/garlic/gnat/gnatdist.gpr @/workspaces/bench-source/src/garlic/gnat/gnatdist.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [74/257] END
echo [75/257] START
cd "/workspaces/bench-source/src/geo_coords"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/geo_coords/geo_coords.gpr @/workspaces/bench-source/src/geo_coords/geo_coords.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [75/257] END
echo [76/257] START
cd "/workspaces/bench-source/src/get_password"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/get_password/get_password.gpr @/workspaces/bench-source/src/get_password/get_password.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [76/257] END
echo [77/257] START
cd "/workspaces/bench-source/src/getopt"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/getopt/getopt.gpr @/workspaces/bench-source/src/getopt/getopt.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [77/257] END
echo [78/257] START
cd "/workspaces/bench-source/src/gid"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gid/gid.gpr @/workspaces/bench-source/src/gid/gid.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [78/257] END
echo [79/257] START
cd "/workspaces/bench-source/src/gnat_math_extensions"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnat_math_extensions/gnat_math_extensions.gpr @/workspaces/bench-source/src/gnat_math_extensions/gnat_math_extensions.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [79/257] END
echo [80/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/gmp"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-bindings/gmp/gnatcoll_gmp.gpr @/workspaces/bench-source/src/gnatcoll-bindings/gmp/gnatcoll_gmp.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [80/257] END
echo [81/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/iconv"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-bindings/iconv/gnatcoll_iconv.gpr @/workspaces/bench-source/src/gnatcoll-bindings/iconv/gnatcoll_iconv.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [81/257] END
echo [82/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/lzma"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-bindings/lzma/gnatcoll_lzma.gpr @/workspaces/bench-source/src/gnatcoll-bindings/lzma/gnatcoll_lzma.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [82/257] END
echo [83/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/omp"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-bindings/omp/gnatcoll_omp.gpr @/workspaces/bench-source/src/gnatcoll-bindings/omp/gnatcoll_omp.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [83/257] END
echo [84/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/python"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-bindings/python/gnatcoll_python.gpr @/workspaces/bench-source/src/gnatcoll-bindings/python/gnatcoll_python.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [84/257] END
echo [85/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/readline"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-bindings/readline/gnatcoll_readline.gpr @/workspaces/bench-source/src/gnatcoll-bindings/readline/gnatcoll_readline.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [85/257] END
echo [86/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/syslog"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-bindings/syslog/gnatcoll_syslog.gpr @/workspaces/bench-source/src/gnatcoll-bindings/syslog/gnatcoll_syslog.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [86/257] END
echo [87/257] START
cd "/workspaces/bench-source/src/gnatcoll-bindings/zlib"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-bindings/zlib/gnatcoll_zlib.gpr @/workspaces/bench-source/src/gnatcoll-bindings/zlib/gnatcoll_zlib.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [87/257] END
echo [88/257] START
cd "/workspaces/bench-source/src/gnatcoll-core"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-core/gnatcoll.gpr @/workspaces/bench-source/src/gnatcoll-core/gnatcoll.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [88/257] END
echo [89/257] START
cd "/workspaces/bench-source/src/gnatcoll-db/postgres"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-db/postgres/gnatcoll_postgres.gpr @/workspaces/bench-source/src/gnatcoll-db/postgres/gnatcoll_postgres.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [89/257] END
echo [90/257] START
cd "/workspaces/bench-source/src/gnatcoll-db/sql"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-db/sql/gnatcoll_sql.gpr @/workspaces/bench-source/src/gnatcoll-db/sql/gnatcoll_sql.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [90/257] END
echo [91/257] START
cd "/workspaces/bench-source/src/gnatcoll-db/sqlite"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-db/sqlite/gnatcoll_sqlite.gpr @/workspaces/bench-source/src/gnatcoll-db/sqlite/gnatcoll_sqlite.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [91/257] END
echo [92/257] START
cd "/workspaces/bench-source/src/gnatcoll-db/xref"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gnatcoll-db/xref/gnatcoll_xref.gpr @/workspaces/bench-source/src/gnatcoll-db/xref/gnatcoll_xref.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [92/257] END
echo [93/257] START
cd "/workspaces/bench-source/src/gpr_unit_provider"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gpr_unit_provider/gpr_unit_provider.gpr @/workspaces/bench-source/src/gpr_unit_provider/gpr_unit_provider.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [93/257] END
echo [94/257] START
cd "/workspaces/bench-source/src/gprbuild"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gprbuild/gpr/gpr.gpr @/workspaces/bench-source/src/gprbuild/gpr/gpr.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [94/257] END
echo [95/257] START
cd "/workspaces/bench-source/src/gtkada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/gtkada/src/gtkada.gpr @/workspaces/bench-source/src/gtkada/src/gtkada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [95/257] END
echo [96/257] START
cd "/workspaces/bench-source/src/hac"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/hac/hac.gpr @/workspaces/bench-source/src/hac/hac.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [96/257] END
echo [97/257] START
cd "/workspaces/bench-source/src/hal"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/hal/hal.gpr @/workspaces/bench-source/src/hal/hal.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [97/257] END
echo [98/257] START
cd "/workspaces/bench-source/src/hangman"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/hangman/hangman.gpr @/workspaces/bench-source/src/hangman/hangman.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [98/257] END
echo [99/257] START
cd "/workspaces/bench-source/src/hello"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/hello/hello.gpr @/workspaces/bench-source/src/hello/hello.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [99/257] END
echo [100/257] START
cd "/workspaces/bench-source/src/hmac"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/hmac/hmac.gpr @/workspaces/bench-source/src/hmac/hmac.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [100/257] END
echo [101/257] START
cd "/workspaces/bench-source/src/honki_tonks_zivilisationen"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/honki_tonks_zivilisationen/honki_tonks_zivilisationen.gpr @/workspaces/bench-source/src/honki_tonks_zivilisationen/honki_tonks_zivilisationen.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [101/257] END
echo [102/257] START
cd "/workspaces/bench-source/src/hungarian"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/hungarian/hungarian.gpr @/workspaces/bench-source/src/hungarian/hungarian.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [102/257] END
echo [103/257] START
cd "/workspaces/bench-source/src/ini_files"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/ini_files/ini_files.gpr @/workspaces/bench-source/src/ini_files/ini_files.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [103/257] END
echo [104/257] START
cd "/workspaces/bench-source/src/inotify"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/inotify/inotify.gpr @/workspaces/bench-source/src/inotify/inotify.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [104/257] END
echo [105/257] START
cd "/workspaces/bench-source/src/inotify"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/inotify/monitor.gpr @/workspaces/bench-source/src/inotify/monitor.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [105/257] END
echo [106/257] START
cd "/workspaces/bench-source/src/j2ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/j2ada/j2ada.gpr @/workspaces/bench-source/src/j2ada/j2ada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [106/257] END
echo [107/257] START
cd "/workspaces/bench-source/src/json/json"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/json/json/json_pretty_print.gpr @/workspaces/bench-source/src/json/json/json_pretty_print.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [107/257] END
echo [108/257] START
cd "/workspaces/bench-source/src/json/json"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/json/json/json.gpr @/workspaces/bench-source/src/json/json/json.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [108/257] END
echo [109/257] START
cd "/workspaces/bench-source/src/jupyter_kernel"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/jupyter_kernel/gnat/jupyter_ada_driver.gpr @/workspaces/bench-source/src/jupyter_kernel/gnat/jupyter_ada_driver.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [109/257] END
echo [110/257] START
cd "/workspaces/bench-source/src/jupyter_kernel"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/jupyter_kernel/gnat/jupyter_ada_kernel.gpr @/workspaces/bench-source/src/jupyter_kernel/gnat/jupyter_ada_kernel.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [110/257] END
echo [111/257] START
cd "/workspaces/bench-source/src/jwt"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/jwt/gnat/jwt.gpr @/workspaces/bench-source/src/jwt/gnat/jwt.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [111/257] END
echo [112/257] START
cd "/workspaces/bench-source/src/labs_radar"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/labs_radar/labs_radar.gpr @/workspaces/bench-source/src/labs_radar/labs_radar.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [112/257] END
echo [113/257] START
cd "/workspaces/bench-source/src/labs_radar/labs_standalone"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/labs_radar/labs_standalone/labs_standalone.gpr @/workspaces/bench-source/src/labs_radar/labs_standalone/labs_standalone.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [113/257] END
echo [114/257] START
cd "/workspaces/bench-source/src/lace"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace/library/lace.gpr @/workspaces/bench-source/src/lace/library/lace.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [114/257] END
echo [115/257] START
cd "/workspaces/bench-source/src/lace_box2d"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace_box2d/library/box2d_thin.gpr @/workspaces/bench-source/src/lace_box2d/library/box2d_thin.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [115/257] END
echo [116/257] START
cd "/workspaces/bench-source/src/lace_bullet"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace_bullet/library/bullet_thin.gpr @/workspaces/bench-source/src/lace_bullet/library/bullet_thin.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [116/257] END
echo [117/257] START
cd "/workspaces/bench-source/src/lace_c_math"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace_c_math/library/c_math_thin.gpr @/workspaces/bench-source/src/lace_c_math/library/c_math_thin.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [117/257] END
echo [118/257] START
cd "/workspaces/bench-source/src/lace_gel_animation_demo"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace_gel_animation_demo/human_rig_demo.gpr @/workspaces/bench-source/src/lace_gel_animation_demo/human_rig_demo.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [118/257] END
echo [119/257] START
cd "/workspaces/bench-source/src/lace_gel_full_demo"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace_gel_full_demo/full_demo.gpr @/workspaces/bench-source/src/lace_gel_full_demo/full_demo.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [119/257] END
# echo [120/257] START
# cd "/workspaces/bench-source/src/lace_shared"
# alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace_shared/lace_shared.gpr @/workspaces/bench-source/src/lace_shared/lace_shared.units &>> /workspaces/bench-source/Adactl_benchmark.output
# echo [120/257] END
echo [121/257] START
cd "/workspaces/bench-source/src/lace/lace_collada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace/lace_collada/library/collada.gpr @/workspaces/bench-source/src/lace/lace_collada/library/collada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [121/257] END
echo [122/257] START
cd "/workspaces/bench-source/src/lace/lace_gel"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace/lace_gel/library/gel.gpr @/workspaces/bench-source/src/lace/lace_gel/library/gel.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [122/257] END
echo [123/257] START
cd "/workspaces/bench-source/src/lace/lace_math"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace/lace_math/library/math.gpr @/workspaces/bench-source/src/lace/lace_math/library/math.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [123/257] END
echo [124/257] START
cd "/workspaces/bench-source/src/lace/lace_opengl"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace/lace_opengl/library/opengl.gpr @/workspaces/bench-source/src/lace/lace_opengl/library/opengl.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [124/257] END
echo [125/257] START
cd "/workspaces/bench-source/src/lace/lace_physics"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace/lace_physics/library/physics.gpr @/workspaces/bench-source/src/lace/lace_physics/library/physics.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [125/257] END
echo [126/257] START
cd "/workspaces/bench-source/src/lace/lace_swig"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace/lace_swig/library/swig.gpr @/workspaces/bench-source/src/lace/lace_swig/library/swig.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [126/257] END
echo [127/257] START
cd "/workspaces/bench-source/src/lace/lace_xml"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lace/lace_xml/library/xml.gpr @/workspaces/bench-source/src/lace/lace_xml/library/xml.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [127/257] END
echo [128/257] START
cd "/workspaces/bench-source/src/lal_highlight"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lal_highlight/highlight.gpr @/workspaces/bench-source/src/lal_highlight/highlight.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [128/257] END
echo [129/257] START
cd "/workspaces/bench-source/src/langkit_support"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/langkit_support/langkit_support.gpr @/workspaces/bench-source/src/langkit_support/langkit_support.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [129/257] END
# echo [130/257] START
# cd "/workspaces/bench-source/src/libadalang"
# alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/libadalang/libadalang.gpr @/workspaces/bench-source/src/libadalang/libadalang.units &>> /workspaces/bench-source/Adactl_benchmark.output
# echo [130/257] END
echo [131/257] START
cd "/workspaces/bench-source/src/libadalang2xml"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/libadalang2xml/libadalang2xml.gpr @/workspaces/bench-source/src/libadalang2xml/libadalang2xml.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [131/257] END
echo [132/257] START
cd "/workspaces/bench-source/src/libgpr2"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/libgpr2/gpr2.gpr @/workspaces/bench-source/src/libgpr2/gpr2.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [132/257] END
# echo [133/257] START
# cd "/workspaces/bench-source/src/libgpr2/gpr2"
# alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/libgpr2/gpr2.gpr @/workspaces/bench-source/src/libgpr2/gpr2.units &>> /workspaces/bench-source/Adactl_benchmark.output
# echo [133/257] END
echo [134/257] START
cd "/workspaces/bench-source/src/libhello"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/libhello/libhello.gpr @/workspaces/bench-source/src/libhello/libhello.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [134/257] END
echo [135/257] START
cd "/workspaces/bench-source/src/libkeccak"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/libkeccak/libkeccak.gpr @/workspaces/bench-source/src/libkeccak/libkeccak.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [135/257] END
echo [136/257] START
cd "/workspaces/bench-source/src/libsimpleio"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/libsimpleio/libsimpleio.gpr @/workspaces/bench-source/src/libsimpleio/libsimpleio.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [136/257] END
echo [137/257] START
cd "/workspaces/bench-source/src/linenoise_ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/linenoise_ada/linenoise.gpr @/workspaces/bench-source/src/linenoise_ada/linenoise.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [137/257] END
echo [138/257] START
cd "/workspaces/bench-source/src/littlefs"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/littlefs/littlefs.gpr @/workspaces/bench-source/src/littlefs/littlefs.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [138/257] END
echo [139/257] START
cd "/workspaces/bench-source/src/lmdb_ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lmdb_ada/lmdb.gpr @/workspaces/bench-source/src/lmdb_ada/lmdb.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [139/257] END
echo [140/257] START
cd "/workspaces/bench-source/src/loga"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/loga/loga.gpr @/workspaces/bench-source/src/loga/loga.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [140/257] END
echo [141/257] START
cd "/workspaces/bench-source/src/lvgl_ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lvgl_ada/lvgl_ada.gpr @/workspaces/bench-source/src/lvgl_ada/lvgl_ada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [141/257] END
echo [142/257] START
cd "/workspaces/bench-source/src/lvgl_ada/lvgl_ada_simulator"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/lvgl_ada/lvgl_ada_simulator/lvgl_ada_simulator.gpr @/workspaces/bench-source/src/lvgl_ada/lvgl_ada_simulator/lvgl_ada_simulator.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [142/257] END
echo [143/257] START
cd "/workspaces/bench-source/src/mage"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/mage/mage.gpr @/workspaces/bench-source/src/mage/mage.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [143/257] END
echo [144/257] START
cd "/workspaces/bench-source/src/mage_hat"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/mage_hat/mage_hat.gpr @/workspaces/bench-source/src/mage_hat/mage_hat.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [144/257] END
echo [145/257] START
cd "/workspaces/bench-source/src/mandelbrot_ascii"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/mandelbrot_ascii/mandelbrot_ascii.gpr @/workspaces/bench-source/src/mandelbrot_ascii/mandelbrot_ascii.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [145/257] END
echo [146/257] START
cd "/workspaces/bench-source/src/markdown"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/markdown/gnat/markdown.gpr @/workspaces/bench-source/src/markdown/gnat/markdown.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [146/257] END
echo [147/257] START
cd "/workspaces/bench-source/src/mathpaqs"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/mathpaqs/mathpaqs.gpr @/workspaces/bench-source/src/mathpaqs/mathpaqs.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [147/257] END
echo [148/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_amf"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_amf.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_amf.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [148/257] END
echo [149/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_amf_dd"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_amf_dd.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_amf_dd.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [149/257] END
echo [150/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_fastcgi"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_fastcgi.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_fastcgi.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [150/257] END
echo [151/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_league"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/packages/alire/matreshka_league/build_matreshka_league.gpr @/workspaces/bench-source/src/matreshka/packages/alire/matreshka_league/build_matreshka_league.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [151/257] END
echo [152/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_servlet"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_servlet.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_servlet.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [152/257] END
echo [153/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_soap"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_soap.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_soap.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [153/257] END
echo [154/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_soap_wsse"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_soap_wsse.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_soap_wsse.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [154/257] END
echo [155/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_spikedog_api"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_spikedog_api.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_spikedog_api.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [155/257] END
echo [156/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_spikedog_core"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_spikedog_core.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_spikedog_core.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [156/257] END
echo [157/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_sql"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_sql.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_sql.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [157/257] END
echo [158/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_sql_firebird"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_sql_firebird.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_sql_firebird.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [158/257] END
echo [159/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_sql_mysql"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_sql_mysql.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_sql_mysql.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [159/257] END
echo [160/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_sql_postgresql"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_sql_postgresql.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_sql_postgresql.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [160/257] END
echo [161/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_sql_sqlite3"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/gnat/matreshka_sql_sqlite3.gpr @/workspaces/bench-source/src/matreshka/gnat/matreshka_sql_sqlite3.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [161/257] END
echo [162/257] START
cd "/workspaces/bench-source/src/matreshka/packages/alire/matreshka_xml"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/matreshka/packages/alire/matreshka_xml/build_matreshka_xml.gpr @/workspaces/bench-source/src/matreshka/packages/alire/matreshka_xml/build_matreshka_xml.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [162/257] END
echo [163/257] START
cd "/workspaces/bench-source/src/mcp2221"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/mcp2221/mcp2221.gpr @/workspaces/bench-source/src/mcp2221/mcp2221.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [163/257] END
echo [164/257] START
cd "/workspaces/bench-source/src/midi"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/midi/midi.gpr @/workspaces/bench-source/src/midi/midi.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [164/257] END
echo [165/257] START
cd "/workspaces/bench-source/src/minirest"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/minirest/minirest.gpr @/workspaces/bench-source/src/minirest/minirest.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [165/257] END
# echo [166/257] START
# cd "/workspaces/bench-source/src/ncursesada"
# alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/ncursesada/Ada95/lib_adacurses.gpr @/workspaces/bench-source/src/ncursesada/Ada95/lib_adacurses.units &>> /workspaces/bench-source/Adactl_benchmark.output
# echo [166/257] END
echo [167/257] START
cd "/workspaces/bench-source/src/openglada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/openglada/opengl.gpr @/workspaces/bench-source/src/openglada/opengl.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [167/257] END
echo [168/257] START
cd "/workspaces/bench-source/src/openglada_glfw"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/openglada_glfw/opengl-glfw.gpr @/workspaces/bench-source/src/openglada_glfw/opengl-glfw.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [168/257] END
echo [169/257] START
cd "/workspaces/bench-source/src/openglada_images"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/openglada_images/opengl-images.gpr @/workspaces/bench-source/src/openglada_images/opengl-images.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [169/257] END
echo [170/257] START
cd "/workspaces/bench-source/src/openglada/openglada_text"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/openglada/openglada_text/opengl-text.gpr @/workspaces/bench-source/src/openglada/openglada_text/opengl-text.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [170/257] END
echo [171/257] START
cd "/workspaces/bench-source/src/optional"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/optional/optional.gpr @/workspaces/bench-source/src/optional/optional.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [171/257] END
echo [172/257] START
cd "/workspaces/bench-source/src/parse_args"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/parse_args/parse_args.gpr @/workspaces/bench-source/src/parse_args/parse_args.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [172/257] END
echo [173/257] START
cd "/workspaces/bench-source/src/partord"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/partord/partord.gpr @/workspaces/bench-source/src/partord/partord.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [173/257] END
echo [174/257] START
cd "/workspaces/bench-source/src/pbkdf2"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/pbkdf2/pbkdf2.gpr @/workspaces/bench-source/src/pbkdf2/pbkdf2.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [174/257] END
echo [175/257] START
cd "/workspaces/bench-source/src/play_2048"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/play_2048/play_2048.gpr @/workspaces/bench-source/src/play_2048/play_2048.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [175/257] END
echo [176/257] START
cd "/workspaces/bench-source/src/powerjoular"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/powerjoular/powerjoular.gpr @/workspaces/bench-source/src/powerjoular/powerjoular.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [176/257] END
echo [177/257] START
cd "/workspaces/bench-source/src/progress_indicators"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/progress_indicators/progress_indicators.gpr @/workspaces/bench-source/src/progress_indicators/progress_indicators.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [177/257] END
echo [178/257] START
cd "/workspaces/bench-source/src/protobuf"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/protobuf/gnat/protoc_gen_ada.gpr @/workspaces/bench-source/src/protobuf/gnat/protoc_gen_ada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [178/257] END
echo [179/257] START
cd "/workspaces/bench-source/src/qoi"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/qoi/qoi.gpr @/workspaces/bench-source/src/qoi/qoi.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [179/257] END
echo [180/257] START
cd "/workspaces/bench-source/src/raspberry_bsp"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/raspberry_bsp/raspberry_bsp.gpr @/workspaces/bench-source/src/raspberry_bsp/raspberry_bsp.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [180/257] END
echo [181/257] START
cd "/workspaces/bench-source/src/rejuvenation"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/rejuvenation/rejuvenation.gpr @/workspaces/bench-source/src/rejuvenation/rejuvenation.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [181/257] END
echo [182/257] START
cd "/workspaces/bench-source/src/remoteio"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/remoteio/remoteio.gpr @/workspaces/bench-source/src/remoteio/remoteio.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [182/257] END
echo [183/257] START
cd "/workspaces/bench-source/src/resources"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/resources/resources.gpr @/workspaces/bench-source/src/resources/resources.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [183/257] END
echo [184/257] START
cd "/workspaces/bench-source/src/rewriters"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/rewriters/rewriters.gpr @/workspaces/bench-source/src/rewriters/rewriters.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [184/257] END
echo [185/257] START
cd "/workspaces/bench-source/src/rsfile"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/rsfile/rsfile.gpr @/workspaces/bench-source/src/rsfile/rsfile.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [185/257] END
echo [186/257] START
cd "/workspaces/bench-source/src/rtmidi"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/rtmidi/rtmidi.gpr @/workspaces/bench-source/src/rtmidi/rtmidi.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [186/257] END
echo [187/257] START
cd "/workspaces/bench-source/src/rxada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/rxada/rxada.gpr @/workspaces/bench-source/src/rxada/rxada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [187/257] END
echo [188/257] START
cd "/workspaces/bench-source/src/saatana"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/saatana/saatana.gpr @/workspaces/bench-source/src/saatana/saatana.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [188/257] END
echo [189/257] START
cd "/workspaces/bench-source/src/scripted_testing"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/scripted_testing/scripted_testing.gpr @/workspaces/bench-source/src/scripted_testing/scripted_testing.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [189/257] END
echo [190/257] START
cd "/workspaces/bench-source/src/sdlada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/sdlada/build/gnat/sdlada.gpr @/workspaces/bench-source/src/sdlada/build/gnat/sdlada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [190/257] END
echo [191/257] START
cd "/workspaces/bench-source/src/semantic_versioning"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/semantic_versioning/semantic_versioning.gpr @/workspaces/bench-source/src/semantic_versioning/semantic_versioning.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [191/257] END
echo [192/257] START
cd "/workspaces/bench-source/src/septum"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/septum/septum.gpr @/workspaces/bench-source/src/septum/septum.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [192/257] END
echo [193/257] START
cd "/workspaces/bench-source/src/sha1"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/sha1/sha1.gpr @/workspaces/bench-source/src/sha1/sha1.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [193/257] END
echo [194/257] START
cd "/workspaces/bench-source/src/sha2"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/sha2/sha2.gpr @/workspaces/bench-source/src/sha2/sha2.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [194/257] END
echo [195/257] START
cd "/workspaces/bench-source/src/si_units"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/si_units/si_units.gpr @/workspaces/bench-source/src/si_units/si_units.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [195/257] END
echo [196/257] START
cd "/workspaces/bench-source/src/simh_tapes"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simh_tapes/simh_tapes.gpr @/workspaces/bench-source/src/simh_tapes/simh_tapes.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [196/257] END
echo [197/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components-connections_server-elv_max_cube.gpr @/workspaces/bench-source/src/simple_components/components-connections_server-elv_max_cube.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [197/257] END
echo [198/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components-connections_server-http_server-sqlite_browser.gpr @/workspaces/bench-source/src/simple_components/components-connections_server-http_server-sqlite_browser.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [198/257] END
echo [199/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components-connections_server-http_server.gpr @/workspaces/bench-source/src/simple_components/components-connections_server-http_server.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [199/257] END
echo [200/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components-connections_server-modbus.gpr @/workspaces/bench-source/src/simple_components/components-connections_server-modbus.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [200/257] END
echo [201/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components-connections_server-mqtt.gpr @/workspaces/bench-source/src/simple_components/components-connections_server-mqtt.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [201/257] END
echo [202/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components-connections_server-secure.gpr @/workspaces/bench-source/src/simple_components/components-connections_server-secure.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [202/257] END
echo [203/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components-connections_server-smtp.gpr @/workspaces/bench-source/src/simple_components/components-connections_server-smtp.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [203/257] END
echo [204/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components-connections_server.gpr @/workspaces/bench-source/src/simple_components/components-connections_server.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [204/257] END
echo [205/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components-gnutls.gpr @/workspaces/bench-source/src/simple_components/components-gnutls.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [205/257] END
echo [206/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components-ntp.gpr @/workspaces/bench-source/src/simple_components/components-ntp.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [206/257] END
echo [207/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components-sqlite.gpr @/workspaces/bench-source/src/simple_components/components-sqlite.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [207/257] END
echo [208/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/components.gpr @/workspaces/bench-source/src/simple_components/components.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [208/257] END
echo [209/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/strings_edit.gpr @/workspaces/bench-source/src/simple_components/strings_edit.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [209/257] END
echo [210/257] START
cd "/workspaces/bench-source/src/simple_components"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_components/tables.gpr @/workspaces/bench-source/src/simple_components/tables.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [210/257] END
echo [211/257] START
cd "/workspaces/bench-source/src/simple_logging"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/simple_logging/simple_logging.gpr @/workspaces/bench-source/src/simple_logging/simple_logging.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [211/257] END
echo [212/257] START
cd "/workspaces/bench-source/src/slip"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/slip/slip.gpr @/workspaces/bench-source/src/slip/slip.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [212/257] END
echo [213/257] START
cd "/workspaces/bench-source/src/socketcan"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/socketcan/src/socketcan.gpr @/workspaces/bench-source/src/socketcan/src/socketcan.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [213/257] END
echo [214/257] START
cd "/workspaces/bench-source/src/spark_unbound"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/spark_unbound/spark_unbound.gpr @/workspaces/bench-source/src/spark_unbound/spark_unbound.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [214/257] END
echo [215/257] START
cd "/workspaces/bench-source/src/sparknacl"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/sparknacl/sparknacl.gpr @/workspaces/bench-source/src/sparknacl/sparknacl.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [215/257] END
echo [216/257] START
cd "/workspaces/bench-source/src/spat"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/spat/spat.gpr @/workspaces/bench-source/src/spat/spat.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [216/257] END
echo [217/257] START
cd "/workspaces/bench-source/src/spdx"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/spdx/spdx.gpr @/workspaces/bench-source/src/spdx/spdx.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [217/257] END
# echo [218/257] START
# cd "/workspaces/bench-source/src/specfun"
# alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/specfun/specfun.gpr @/workspaces/bench-source/src/specfun/specfun.units &>> /workspaces/bench-source/Adactl_benchmark.output
# echo [218/257] END
echo [219/257] START
cd "/workspaces/bench-source/src/spoon"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/spoon/spoon.gpr @/workspaces/bench-source/src/spoon/spoon.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [219/257] END
echo [220/257] START
cd "/workspaces/bench-source/src/startup_gen"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/startup_gen/startup_gen.gpr @/workspaces/bench-source/src/startup_gen/startup_gen.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [220/257] END
echo [221/257] START
cd "/workspaces/bench-source/src/stephes_ada_library"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/stephes_ada_library/build/stephes_ada_library.gpr @/workspaces/bench-source/src/stephes_ada_library/build/stephes_ada_library.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [221/257] END
echo [222/257] START
cd "/workspaces/bench-source/src/stopwatch"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/stopwatch/stopwatch.gpr @/workspaces/bench-source/src/stopwatch/stopwatch.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [222/257] END
echo [223/257] START
cd "/workspaces/bench-source/src/svd2ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/svd2ada/svd2ada.gpr @/workspaces/bench-source/src/svd2ada/svd2ada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [223/257] END
echo [224/257] START
cd "/workspaces/bench-source/src/system_random"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/system_random/system_random.gpr @/workspaces/bench-source/src/system_random/system_random.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [224/257] END
echo [225/257] START
cd "/workspaces/bench-source/src/tash"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/tash/tash.gpr @/workspaces/bench-source/src/tash/tash.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [225/257] END
echo [226/257] START
cd "/workspaces/bench-source/src/templates_parser"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/templates_parser/templates_parser.gpr @/workspaces/bench-source/src/templates_parser/templates_parser.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [226/257] END
echo [227/257] START
cd "/workspaces/bench-source/src/tiled_code_gen"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/tiled_code_gen/tiled_code_gen.gpr @/workspaces/bench-source/src/tiled_code_gen/tiled_code_gen.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [227/257] END
echo [228/257] START
cd "/workspaces/bench-source/src/tiny_text"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/tiny_text/tiny_text.gpr @/workspaces/bench-source/src/tiny_text/tiny_text.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [228/257] END
echo [229/257] START
cd "/workspaces/bench-source/src/tlsada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/tlsada/tlsada.gpr @/workspaces/bench-source/src/tlsada/tlsada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [229/257] END
echo [230/257] START
cd "/workspaces/bench-source/src/toml_slicer"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/toml_slicer/toml_slicer.gpr @/workspaces/bench-source/src/toml_slicer/toml_slicer.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [230/257] END
echo [231/257] START
cd "/workspaces/bench-source/src/trendy_terminal"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/trendy_terminal/trendy_terminal.gpr @/workspaces/bench-source/src/trendy_terminal/trendy_terminal.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [231/257] END
echo [232/257] START
cd "/workspaces/bench-source/src/trendy_test"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/trendy_test/trendy_test.gpr @/workspaces/bench-source/src/trendy_test/trendy_test.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [232/257] END
echo [233/257] START
cd "/workspaces/bench-source/src/uri_ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/uri_ada/uri_ada.gpr @/workspaces/bench-source/src/uri_ada/uri_ada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [233/257] END
echo [234/257] START
cd "/workspaces/bench-source/src/uri_mime"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/uri_mime/uri_mime.gpr @/workspaces/bench-source/src/uri_mime/uri_mime.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [234/257] END
echo [235/257] START
cd "/workspaces/bench-source/src/usb_embedded"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/usb_embedded/usb_embedded.gpr @/workspaces/bench-source/src/usb_embedded/usb_embedded.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [235/257] END
echo [236/257] START
cd "/workspaces/bench-source/src/utf8test"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/utf8test/utf8test.gpr @/workspaces/bench-source/src/utf8test/utf8test.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [236/257] END
echo [237/257] START
cd "/workspaces/bench-source/src/vaton"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/vaton/vaton.gpr @/workspaces/bench-source/src/vaton/vaton.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [237/257] END
echo [238/257] START
cd "/workspaces/bench-source/src/virtapu"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/virtapu/virtapu.gpr @/workspaces/bench-source/src/virtapu/virtapu.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [238/257] END
echo [239/257] START
cd "/workspaces/bench-source/src/weechat_ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/weechat_ada/weechat_ada.gpr @/workspaces/bench-source/src/weechat_ada/weechat_ada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [239/257] END
echo [240/257] START
cd "/workspaces/bench-source/src/wordle"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/wordle/wordle.gpr @/workspaces/bench-source/src/wordle/wordle.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [240/257] END
echo [241/257] START
cd "/workspaces/bench-source/src/wordlelib"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/wordlelib/wordlelib.gpr @/workspaces/bench-source/src/wordlelib/wordlelib.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [241/257] END
echo [242/257] START
cd "/workspaces/bench-source/src/wordlist"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/wordlist/wordlist.gpr @/workspaces/bench-source/src/wordlist/wordlist.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [242/257] END
echo [243/257] START
cd "/workspaces/bench-source/src/workers"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/workers/workers.gpr @/workspaces/bench-source/src/workers/workers.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [243/257] END
echo [244/257] START
cd "/workspaces/bench-source/src/xdg_base_dir"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/xdg_base_dir/xdg_base_dir.gpr @/workspaces/bench-source/src/xdg_base_dir/xdg_base_dir.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [244/257] END
echo [245/257] START
cd "/workspaces/bench-source/src/xia"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/xia/XIA.gpr @/workspaces/bench-source/src/xia/XIA.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [245/257] END
echo [246/257] START
cd "/workspaces/bench-source/src/xml_ez_out"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/xml_ez_out/xml_ez_out.gpr @/workspaces/bench-source/src/xml_ez_out/xml_ez_out.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [246/257] END
echo [247/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/xmlada/distrib/xmlada.gpr @/workspaces/bench-source/src/xmlada/distrib/xmlada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [247/257] END
echo [248/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/xmlada/dom/xmlada_dom.gpr @/workspaces/bench-source/src/xmlada/dom/xmlada_dom.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [248/257] END
echo [249/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/xmlada/input_sources/xmlada_input.gpr @/workspaces/bench-source/src/xmlada/input_sources/xmlada_input.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [249/257] END
echo [250/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/xmlada/sax/xmlada_sax.gpr @/workspaces/bench-source/src/xmlada/sax/xmlada_sax.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [250/257] END
echo [251/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/xmlada/schema/xmlada_schema.gpr @/workspaces/bench-source/src/xmlada/schema/xmlada_schema.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [251/257] END
echo [252/257] START
cd "/workspaces/bench-source/src/xmlada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/xmlada/unicode/xmlada_unicode.gpr @/workspaces/bench-source/src/xmlada/unicode/xmlada_unicode.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [252/257] END
echo [253/257] START
cd "/workspaces/bench-source/src/xoshiro"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/xoshiro/xoshiro.gpr @/workspaces/bench-source/src/xoshiro/xoshiro.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [253/257] END
echo [254/257] START
cd "/workspaces/bench-source/src/yeison"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/yeison/yeison.gpr @/workspaces/bench-source/src/yeison/yeison.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [254/257] END
echo [255/257] START
cd "/workspaces/bench-source/src/zeromq_ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/zeromq_ada/zmq.gpr @/workspaces/bench-source/src/zeromq_ada/zmq.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [255/257] END
echo [256/257] START
cd "/workspaces/bench-source/src/zipada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/zipada/zipada.gpr @/workspaces/bench-source/src/zipada/zipada.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [256/257] END
echo [257/257] START
cd "/workspaces/bench-source/src/zlib_ada"
alr exec -- adactl -f /workspaces/bench-source/benchmark-rules/all_rules_in_one_file/_all.aru -p /workspaces/bench-source/src/zlib_ada/zlib.gpr @/workspaces/bench-source/src/zlib_ada/zlib.units &>> /workspaces/bench-source/Adactl_benchmark.output
echo [257/257] END
