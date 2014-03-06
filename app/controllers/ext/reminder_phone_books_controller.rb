module Ext
 class ReminderPhoneBooksController  < ExtApplicationController

 	def index
 		load_project params[:project_id]
 		respond_to do |format|
			format.html
  		format.json { render json: @project.ext_reminder_phone_books }
  	end
 	end


 	def new
 		load_project params[:project_id]
 		@reminder = Ext::ReminderPhoneBook.new
 	end

 	def create
 		load_project params[:project_id]
 		@reminder = @project.ext_reminder_phone_books.build(params[:ext_reminder_phone_book])
 		if 	@reminder.save
 			flash[:notice] = I18n.t("controllers.reminder_phone_books.successfully_created")
 			redirect_to :action => "index"
 		else
 			render :new
 		end
 	end

 	def edit
 		load_project params[:project_id]
 		begin
 			@reminder = @project.ext_reminder_phone_books.find(params[:id])
 		rescue
 			flash[:error] = I18n.t("controllers.reminder_phone_books.invalid")
 			redirect_to :action => :index
 		end	
 	end

 	def update
 		load_project params[:project_id]
 		begin
 			@reminder = @project.ext_reminder_phone_books.find(params[:id])
	 		if @reminder.update_attributes(params[:ext_reminder_phone_book])
	 			flash[:notice] = I18n.t("controllers.reminder_phone_books.successfully_updated")
	 			redirect_to :action => :index 
	 		else		
	 			render :edit
 			end
 		rescue Exception => e
 			flash[:error] = e.message
 			redirect_to :action => :index
 		end
 	end

 	def update_reminder_phone_book_types
 		load_project params[:project_id]
    if @project.update_attributes(params[:project])
      redirect_to ext_project_reminder_phone_books_path(@project), notice: I18n.t("controllers.reminder_phone_books.contact_group_successfully_updated")
    else
      redirect_to ext_project_reminder_phone_books_path(@project), flash: { error: I18n.t("controllers.reminder_phone_books.contact_group_update_error")}
    end
  end

 	def destroy
 		load_project params[:project_id]
 		begin
 			@reminder = @project.ext_reminder_phone_books.find(params[:id])
	 		if @reminder.destroy
	 			flash[:notice] = I18n.t("controllers.reminder_phone_books.successfully_deleted", :contact => @reminder.phone_number)
	 		else
	 			flash[:error] = I18n.t("controllers.reminder_phone_books.delete_error", :contact => @reminder.phone_number)
	 		end
 		rescue Exception => e
 			flash[:error] = e.message
 		ensure	
 			redirect_to :action => :index 
 		end
 	end

 end

end