package main

import (
	"fmt"
	"os"
)

func main() {

	var c []byte = make([]byte, 1)

	for {
		_, err := os.Stdin.Read(c)
		if err != nil {
			fmt.Println("Failed to read:", err)
			break
		}

		if c[0] == 'q' {
			break
		}
	}

}
