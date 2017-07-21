#!/bin/sh
cd `dirname $0`
exec erl -sname cluster_report -config $PWD/sys.config -pa $PWD/_build/default/lib/*/ebin -boot start_sasl -setcookie cluster_report -s cluster_report start -proto_dist hawk_tcp -hidden