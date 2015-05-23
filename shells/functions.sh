#!/bin/bash

function github_url() {
    echo https://$(ruby -e "puts '$(git config --get remote.origin.url)'.scan(/git@(.+)[\/\:]{1}(.+\/.+)(?:.git)*/).join('/')")
}

function pr_page_url() {
    echo $(github_url)/pulls/$(git rev-parse --abbrev-ref HEAD)
}

$1
