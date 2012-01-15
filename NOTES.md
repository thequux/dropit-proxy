This failed to parse the reference grammar, because the "elements"
parser is greedy, and parses all the way through the rule name for the
second rule, but won't let go of it. As it turns out, this is why we
have things other than LL(1). We should rewrite the parser using
Happy.