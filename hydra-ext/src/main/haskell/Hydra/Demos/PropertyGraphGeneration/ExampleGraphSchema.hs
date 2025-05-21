module Hydra.Demos.PropertyGraphGeneration.ExampleGraphSchema where

import Hydra.Core
import Hydra.Dsl.Pg.Schemas
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types


dateType = Types.string
decimalType = Types.float64

-- Vertex labels -----------------------

employeeVertexLabel = "Employee"
departmentVertexLabel = "Department"
customerVertexLabel = "Customer"
productVertexLabel = "Product"
saleVertexLabel = "Sale"
saleItemVertexLabel = "SaleItem"
customerInteractionVertexLabel = "CustomerInteraction"

-- Edge labels -------------------------

managesEdgeLabel = "manages"
belongsToEdgeLabel = "belongsTo"
parentDepartmentEdgeLabel = "parentDepartment"
soldEdgeLabel = "sold"
purchasedEdgeLabel = "purchased"
includesEdgeLabel = "includes"
containsProductEdgeLabel = "containsProduct"
employeeEdgeLabel = "employee"
customerEdgeLabel = "customer"

-- Interaction types -------------------

callInteractionType = "Call"
emailInteractionType = "Email"
meetingInteractionType = "Meeting"

-- Graph Schema ------------------------

salesGraphSchema = schema vertexTypes edgeTypes
  where
    vertexTypes = [
      vertexType employeeVertexLabel Types.int32 [
        required $ propertyType "firstName" Types.string,
        required $ propertyType "lastName" Types.string,
        required $ propertyType "email" Types.string,
        propertyType "hireDate" dateType],

      vertexType departmentVertexLabel Types.int32 [
        required $ propertyType "name" Types.string],

      vertexType customerVertexLabel Types.int32 [
        required $ propertyType "companyName" Types.string,
        propertyType "contactName" Types.string,
        propertyType "email" Types.string,
        propertyType "phone" Types.string],

      vertexType productVertexLabel Types.int32 [
        required $ propertyType "name" Types.string,
        propertyType "price" decimalType],

      vertexType saleVertexLabel Types.int32 [
        required $ propertyType "saleDate" dateType,
        propertyType "totalAmount" decimalType],

      vertexType saleItemVertexLabel Types.int32 [
        propertyType "quantity" Types.int32,
        propertyType "itemPrice" decimalType],

      vertexType customerInteractionVertexLabel Types.string [
        required $ propertyType "interactionDate" dateType,
        required $ propertyType "interactionType" Types.string,
        propertyType "notes" Types.string,
        propertyType "durationMinutes" Types.int32,
        propertyType "followUpRequired" Types.boolean,
        propertyType "customerInitiated" Types.boolean]]

    edgeTypes = [
      simpleEdgeType managesEdgeLabel employeeVertexLabel employeeVertexLabel [
        propertyType "sinceDate" dateType],

      simpleEdgeType belongsToEdgeLabel employeeVertexLabel departmentVertexLabel [
        required $ propertyType "joinDate" dateType,
        propertyType "role" Types.string],

      simpleEdgeType parentDepartmentEdgeLabel departmentVertexLabel departmentVertexLabel [
        propertyType "since" dateType],

      simpleEdgeType "hasCategory" productVertexLabel "Category" [
        propertyType "sinceDate" dateType,
        required $ propertyType "isPrimary" Types.boolean],

      simpleEdgeType soldEdgeLabel employeeVertexLabel saleVertexLabel [
        required $ propertyType "commissionRate" Types.float64,
        propertyType "salesChannel" Types.string],

      simpleEdgeType purchasedEdgeLabel customerVertexLabel saleVertexLabel [
        required $ propertyType "paymentMethod" Types.string,
        propertyType "satisfactionRating" Types.int32,
        propertyType "isRepeatCustomer" Types.boolean],

      simpleEdgeType includesEdgeLabel saleVertexLabel saleItemVertexLabel [
        required $ propertyType "itemOrder" Types.int32,
        propertyType "discountApplied" Types.float64],

      simpleEdgeType containsProductEdgeLabel saleItemVertexLabel productVertexLabel [
        propertyType "warrantyPeriodYears" Types.int32],

      simpleEdgeType employeeEdgeLabel customerInteractionVertexLabel employeeVertexLabel [],

      simpleEdgeType customerEdgeLabel customerInteractionVertexLabel customerVertexLabel []]
