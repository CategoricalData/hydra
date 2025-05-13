module Hydra.Demos.PropertyGraphGeneration.ExampleSchema where

import Hydra.Core
import Hydra.Dsl.Pg.Schemas
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types


dateType = Types.string
decimalType = Types.float64

salesSchema = schema vertexTypes edgeTypes
  where
    vertexTypes = [
      vertexType "Employee" Types.int32 [
        required $ propertyType "firstName" Types.string,
        required $ propertyType "lastName" Types.string,
        required $ propertyType "email" Types.string,
        propertyType "hireDate" dateType],

      vertexType "Department" Types.int32 [
        required $ propertyType "name" Types.string],

      vertexType "Customer" Types.int32 [
        required $ propertyType "companyName" Types.string,
        propertyType "contactName" Types.string,
        propertyType "email" Types.string,
        propertyType "phone" Types.string],

      vertexType "Product" Types.int32 [
        required $ propertyType "name" Types.string,
        propertyType "price" decimalType],

      vertexType "Sale" Types.int32 [
        required $ propertyType "saleDate" dateType,
        propertyType "totalAmount" decimalType],

      vertexType "SaleItem" Types.int32 [
        propertyType "quantity" Types.int32,
        propertyType "itemPrice" decimalType],

      vertexType "CustomerInteraction" Types.string [
        required $ propertyType "interactionDate" dateType,
        required $ propertyType "interactionType" Types.string,
        propertyType "notes" Types.string,
        propertyType "durationMinutes" Types.int32,
        propertyType "followUpRequired" Types.boolean,
        propertyType "customerInitiated" Types.boolean]]

    edgeTypes = [
      simpleEdgeType "manages" "Employee" "Employee" [
        propertyType "sinceDate" dateType],

      simpleEdgeType "belongsTo" "Employee" "Department" [
        required $ propertyType "joinDate" dateType,
        propertyType "role" Types.string],

      simpleEdgeType "parentDepartment" "Department" "Department" [
        propertyType "since" dateType],

      simpleEdgeType "hasCategory" "Product" "Category" [
        propertyType "sinceDate" dateType,
        required $ propertyType "isPrimary" Types.boolean],

      simpleEdgeType "sold" "Employee" "Sale" [
        required $ propertyType "commissionRate" Types.float64,
        propertyType "salesChannel" Types.string],

      simpleEdgeType "purchased" "Customer" "Sale" [
        required $ propertyType "paymentMethod" Types.string,
        propertyType "satisfactionRating" Types.int32,
        propertyType "isRepeatCustomer" Types.boolean],

      simpleEdgeType "includes" "Sale" "SaleItem" [
        required $ propertyType "itemOrder" Types.int32,
        propertyType "discountApplied" Types.float64],

      simpleEdgeType "containsProduct" "SaleItem" "Product" [
        propertyType "warrantyPeriod" Types.int32],

      simpleEdgeType "employee" "CustomerInteraction" "Employee" [],

      simpleEdgeType "customer" "CustomerInteraction" "Customer" []]
