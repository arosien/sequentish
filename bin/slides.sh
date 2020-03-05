#!/bin/bash

asciidoctor-revealjs -r asciidoctor-diagram -D target/asciidoctor -a revealjsdir=https://cdn.jsdelivr.net/npm/reveal.js@3.9.2 src/main/asciidoctor/slides.adoc 