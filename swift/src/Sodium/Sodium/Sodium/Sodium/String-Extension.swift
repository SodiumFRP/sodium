//
//  String-Extension.swift
//  Sodium
//
//  Created by Andrew Bradnan on 5/6/16.
//  Copyright © 2016 Whirlygig Ventures. All rights reserved.
//

import Foundation

extension String {
    var isUpperCase: Bool {
        return self == self.uppercaseString
    }
    var isLowerCase: Bool {
        return self == self.lowercaseString
    }
}