#!/bin/bash

# Time-stamp: <08/03/2009 10:02:41 星期一 by ahei>

# @file (>>>FILE<<<)
# @version 1.0
# @author ahei

readonly PROGRAM_NAME="(>>>FILE<<<)"
readonly PROGRAM_VERSION="1.0.0"

home=`dirname "$0"`
home=`cd "$home" && pwd`

. "$home"/common.sh

usage()
{
    code=1
    if [ $# -gt 0 ]; then
        code="$1"
    fi

    if [ "$code" != 0 ]; then
        redirect="1>&2"
    fi

    eval cat "$redirect" << EOF
usage: ${PROGRAM_NAME} [OPTIONS]

Options:
    -i [<INSTALL_DIR>]
        Install this shell script to your machine, INSTALL_DIR default is /usr/bin.
    -v  Output version info.
    -h  Output this help.
EOF

    exit "$code"
}

version()
{
    echoo "${PROGRAM_NAME} ${PROGRAM_VERSION}"
    exit
}

while getopts ":hvi:" OPT; do
    case "$OPT" in
        i)
            install "$OPTARG"
            ;;
            
        v)
            version
            ;;

        h)
            usage 0
            ;;

        :)
            case "${OPTARG}" in
                i)
                    install
                    ;;
                
                ?)
                    echoe "Option \`-${OPTARG}' need argument.\n"
                    usage
            esac
            ;;

        ?)
            echoe "Invalid option \`-${OPTARG}'.\n"
            usage
            ;;
    esac
done

shift $((OPTIND - 1))

(>>>POINT<<<)
