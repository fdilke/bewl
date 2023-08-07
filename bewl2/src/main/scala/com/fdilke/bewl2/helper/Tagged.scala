package com.fdilke.bewl2.helper

class Tagged[PAYLOAD, TAG](
  payload: PAYLOAD
) extends AnyVal:
  inline def untag: PAYLOAD =
    payload


object Tagged:
  extension[PAYLOAD](
    payload: PAYLOAD
  )
    def tag[TAG]: PAYLOAD Tagged TAG =
      Tagged[PAYLOAD, TAG](payload)
