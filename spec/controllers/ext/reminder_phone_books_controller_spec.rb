require 'spec_helper'

describe Ext::ReminderPhoneBooksController
  # include Devise::TestHelpers

  # let!(:account) { Account.make }
  # let!(:project) { Project.make :account => account }

  # before(:each) do
  #   sign_in account
  # end

  # it "should create reminder phone book when attributes are valid" do
  #   post :create, project_id: project.id, ext_reminder_phone_book: {phone_number: '0123456789'}
  #   flash[:notice].should eq(I18n.t("controllers.reminder_phone_books.successfully_created"))
  #   response.should be_redirect
  # end

  # it "should not create reminder phone book when attributes are invalid" do
  #   post :create, project_id: project.id, ext_reminder_phone_book: {phone_number: ''}
  #   response.should be_success
  #   response.should render_template("new")
  # end

  # it "should update reminder phone book when attributes are valid" do
  #   reminder_phone_book = Ext::ReminderPhoneBook.create phone_number: '012345678', project_id: project.id
  #   put :update, project_id: project.id, id: reminder_phone_book.id, ext_reminder_phone_book: {phone_number: '0123456789'}
  #   flash[:notice].should eq(I18n.t("controllers.reminder_phone_books.successfully_updated"))
  #   response.should be_redirect
  # end

  # it "should not update reminder phone book when attributes are invalid" do
  #   reminder_phone_book = Ext::ReminderPhoneBook.create phone_number: '012345678', project_id: project.id
  #   put :update, project_id: project.id, id: reminder_phone_book.id, ext_reminder_phone_book: {phone_number: ''}
  #   response.should be_success
  #   response.should render_template("edit")
  # end

  # it "should destroy reminder phone book" do
  #   reminder_phone_book = Ext::ReminderPhoneBook.create phone_number: '012345678', project_id: project.id
  #   delete :destroy, project_id: project.id, id: reminder_phone_book.id
  #   flash[:notice].should eq(I18n.t("controllers.reminder_phone_books.successfully_deleted", contact: reminder_phone_book.phone_number))
  #   response.should be_redirect
  # end

  # context "update reminder phone book types" do
  #   it "should create new ext reminder phone book types" do
  #     params = {
  #       :ext_reminder_phone_book_types_attributes => {
  #         '0' => {
  #           'name'=> "bar",
  #         }
  #       }
  #     }

  #     put :update_reminder_phone_book_types, :project_id => project.id, :project => params
  #     response.should be_redirect
  #     flash[:notice].should eq(I18n.t("controllers.reminder_phone_books.contact_group_successfully_updated"))
  #     Ext::ReminderPhoneBookType.count.should be(1)
  #   end

  #   it "should update an existing ext reminder phone book types" do
  #     reminder_phone_book_type = Ext::ReminderPhoneBookType.create :name => "bar", :project_id => project.id
  #     params = {
  #       :ext_reminder_phone_book_types_attributes => {
  #         '0' => {
  #           'name' => "foo",
  #           'id' => reminder_phone_book_type.id
  #         }
  #       }
  #     }

  #     Ext::ReminderPhoneBookType.count.should be(1)
  #     put :update_reminder_phone_book_types, :project_id => project.id, :project => params
  #     response.should be_redirect
  #     flash[:notice].should eq(I18n.t("controllers.reminder_phone_books.contact_group_successfully_updated"))
  #     Ext::ReminderPhoneBookType.first.name.should eq("foo")
  #     Ext::ReminderPhoneBookType.count.should be(1)
  #   end
  # end
# end