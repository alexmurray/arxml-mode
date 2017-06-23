#!/usr/bin/env python3

import sys
import argparse
import xml.parsers.expat


def index_arxml(f, outfile):
    stack = []
    parser = xml.parsers.expat.ParserCreate()

    def start(tag, attrib):
        stack.append({'tag': tag,
                      'ref': None,
                      'short_name': None,
                      'line': None,
                      'col': None})

    def end(tag):
        assert(tag == stack[-1]['tag'])
        if stack[-1]['short_name'] is not None:
            outfile.write('d %s %s %d %d\n' % ('/' +
                                               '/'.join(tag['short_name']
                                                        for tag in stack
                                                        if tag['short_name']
                                                        is not None),
                                               f.name,
                                               stack[-1]['line'],
                                               stack[-1]['col']))
        if stack[-1]['ref'] is not None:
            outfile.write('r %s %s %d %d\n' % (stack[-1]['ref'],
                                               f.name,
                                               stack[-1]['line'],
                                               stack[-1]['col']))
        stack.pop()

    def data(data):
        assert(len(stack) > 0)
        # ignore any shortnames in the SYMBOL-PROPS
        if ((stack[-1]['tag'] == 'SHORT-NAME') and (stack[-2]['tag'] != 'SYMBOL-PROPS')):
            # assign the short_name to the parent element
            stack[-2]['short_name'] = data
            stack[-2]['line'] = parser.CurrentLineNumber
            stack[-2]['col'] = parser.CurrentColumnNumber
        # get references to short-names
        if (stack[-1]['tag'].endswith('REF')):
            stack[-1]['ref'] = data
            stack[-1]['line'] = parser.CurrentLineNumber
            stack[-1]['col'] = parser.CurrentColumnNumber

    parser.StartElementHandler = start
    parser.EndElementHandler = end
    parser.CharacterDataHandler = data
    parser.Parse(f.read())


parser = argparse.ArgumentParser(description='Parse and index arxml files.')
parser.add_argument('arxml', nargs='+', type=argparse.FileType('r'),
                    default=sys.stdin)
parser.add_argument('-f', '--outfile', type=argparse.FileType('w'),
                    default=sys.stdout)
args = parser.parse_args()
for f in args.arxml:
    index_arxml(f, args.outfile)
