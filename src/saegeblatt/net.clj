(ns saegeblatt.net
  (:import (java.net DatagramPacket DatagramSocket InetAddress)))

(defn make-socket 
  ([] (new DatagramSocket))
  ([port] (new DatagramSocket port)))

(defn send-data [send-socket ip port byte-seq]
  (let [ipaddress (InetAddress/getByName ip)
        send-packet (new DatagramPacket (byte-array byte-seq) (count byte-seq) ipaddress port)]
    (.send send-socket send-packet)))

(defn push [send-socket ip port]
  (send-data send-socket ip port []))

(defn receive-data [receive-socket]
  (let [receive-data (byte-array 1)
        receive-packet (new DatagramPacket receive-data 1)]
    (.receive receive-socket receive-packet)
    (first (.getData receive-packet))))

(defn make-receive [receive-port]
  (let [receive-socket (make-socket receive-port)]
    (fn [] (receive-data receive-socket))))

(defn make-send [ip port]
  (let [send-socket (make-socket)]
    (fn
      ([data] (send-data send-socket ip port [data]))
      ([] (push send-socket ip port)))))

