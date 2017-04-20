tree trans:
  S {
    np![case=nom][]
    vp {
      v+
      np![case=acc][]
    }
}


tree intrans:
  S {
    np![case=nom][]
    v+
  }


tree np_n:
  np[][case=?case] {
    det! [case=?case][]
    n+   [case=?case][]
  }

tree aux_adj:
  n [][case=?case] {
    adj+ [case=?case][]
    n* [case=?case][]
  }
    

tree det:
  det+


word 'jagt': trans

word 'hund': np_n[case=nom]
word 'hund': np_n[case=acc]

word 'hase': np_n[case=nom]
word 'hasen': np_n[case=acc]

word 'der': det[case=nom]
word 'den': det[case=acc]

lemma 'schnell': aux_adj {
  word 'schnelle': [case=nom]
  word 'schnellen': [case=acc]
}

