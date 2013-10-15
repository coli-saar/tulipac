tree intrans:
  s {
    np![num=sg]
    v+
  }

tree pns:
  np[][num=sg] {
    pn+
  }

tree pnp:
  np[][num=pl] {
    n+
  }

word 'sleeps': intrans
word 'peter': pns
word 'dogs': pnp

