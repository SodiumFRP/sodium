//
//  Array-Extension.swift
//  Sodium
//
//  Created by Andrew Bradnan on 5/2/16.
//  Copyright © 2016 Whirlygig Ventures. All rights reserved.
//

import Foundation

extension Array {
    func indexOf(includedElement: Element -> Bool) -> Int? {
        for (idx, element) in self.enumerate() {
            if includedElement(element) {
                return idx
            }
        }
        return nil
    }
}

extension Array where Element : Equatable {

    func indexOf(e: Element) -> Int? {
        for (idx, element) in self.enumerate() {
            if element == e {
                return idx
            }
        }
        return nil
    }
    
    mutating func remove(e: Element) -> Bool {
        if let idx = self.indexOf(e) {
            self.removeAtIndex(idx)
            return true
        }
        return false
    }
    
}
